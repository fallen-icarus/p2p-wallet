{-# LANGUAGE DuplicateRecordFields #-}

module P2PWallet.Actions.Utils where

import System.Exit (ExitCode(ExitSuccess))
import System.Process (readCreateProcessWithExitCode, shell)
import Data.Map qualified as Map
import Data.Maybe (fromJust)

import P2PWallet.Data.App
import P2PWallet.Data.Core.Bech32Address
import P2PWallet.Data.Core.Asset
import P2PWallet.Data.Core.Network
import P2PWallet.Data.Core.Witness
import P2PWallet.Data.Koios.AddressUTxO
import P2PWallet.Data.Lens hiding (network,wallets)
import P2PWallet.Data.Plutus
import P2PWallet.Data.Wallets
import P2PWallet.Prelude

-- | Run an action. If successfull, do something with the results. If unsuccessfull, do something
-- with the error message. This is usefull for letting the frontend decide what to do based on
-- the results.
runAction :: (Text -> b) -> (c -> b) -> IO c -> IO b
runAction onAppErr onSuccess action = 
  handle (\(AppError err) -> return $ onAppErr err) $ fmap onSuccess action

-- | Run an action. If successfull, do something with the results. If unsuccessfull, pass
-- the error message to `Alert`. 
runActionOrAlert :: (c -> AppEvent) -> IO c -> IO AppEvent
runActionOrAlert onSuccess action = runAction Alert onSuccess action

-- | Gets the `Just` value or throws an `AppError` with the supplied error message.
fromJustOrAppError :: Text -> Maybe a -> IO a
fromJustOrAppError errMsg = maybe (throwIO $ AppError errMsg) return

-- | Gets the `Right` value or throws an `AppError` with the error message in the `Left`.
fromRightOrAppError :: Either Text a -> IO a
fromRightOrAppError = either (throwIO . AppError) return

-- | Run the command and return either the results or the error message. The trailing newline is
-- dropped.
runCmd :: String -> IO String
runCmd cmd =
  readCreateProcessWithExitCode (shell cmd) [] >>= \case
    (ExitSuccess,result,_) -> return $ dropTrailingNewline result
    (_,_,errMsg) -> throwIO $ AppError $ toText $ dropTrailingNewline errMsg
  where
    dropTrailingNewline s = 
      case reverse s of
        ('\n':xs) -> reverse xs
        _ -> s

-- | Convert `Network` to the required flag for cardano-cli.
toNetworkFlag :: Network -> Text
toNetworkFlag Mainnet = "--mainnet"
toNetworkFlag Testnet = "--testnet-magic 1"

-- | Re-index the lists while preserving ordering.
reIndex :: [(Int,a)] -> [(Int,a)]
reIndex xs = go xs 1
  where
    go [] _ = []
    go ((_,y):ys) i = (i,y) : go ys (i+1)

-- | Balance the inputs with the outputs by updating the changeOutput and subtracting off the
-- fee.
balanceTx :: TxBuilderModel -> TxBuilderModel
balanceTx tx@TxBuilderModel{_inputs,_outputs,_txFee,_certificates} =
    -- let (loves,assets) = change
    tx & changeOutput . lovelaces .~ loves - _txFee - requiredDeposits
       & changeOutput . nativeAssets .~ assets
  where
    -- The total deposit required from certificates.
    requiredDeposits :: Lovelace
    requiredDeposits = (flip . flip foldl') 0 _certificates $ \acc (_,cert) ->
      case cert ^. certificateAction of
        Registration -> acc + 2_000_000 -- 2 ADA must be paid.
        Deregistration -> acc - 2_000_000 -- 2 ADA must be returned.
        Delegation _ -> acc

    -- The amount of ADA and native assets available as change.
    (loves :: Lovelace, assets :: [NativeAsset]) =
      let (inLoves,inAssets) = 
            ( sum $ map (view lovelaces . snd) _inputs
            , concatMap (view nativeAssets . snd) _inputs
            )
          (outLoves,outAssets) = 
            ( sum $ map (view lovelaces . snd) _outputs
            , concatMap (view nativeAssets . snd) _outputs
            )
          inMap = Map.fromList $ map (\a -> (a ^. to fullAssetName, a ^. quantity)) inAssets
          outMap = Map.fromList $ map (\a -> (a ^. to fullAssetName, -(a ^. quantity))) outAssets
          bal = map (fromJust . readNativeAsset . \(name',q) -> show q <> " " <> name') 
              $ filter (\(_,q) -> q /= 0)
              $ Map.toList 
              $ Map.unionWith (+) inMap outMap
      in (inLoves - outLoves,bal)

-- | A list of unique pub keys that must sign the transaction as well as their `DerivationPath`
-- if known. Read the documentation for `RegistrationWitness` to see why they are treated
-- separately.
requiredWitnesses :: TxBuilderModel -> Either Text ([NormalWitness],[RegistrationWitness])
requiredWitnesses TxBuilderModel{_inputs,_certificates} = do
    -- | Get the required pubkeys among the inputs. Returns the first error, if any.
    inputWitnesses <- catMaybes <$> sequence (map checkInput _inputs)

    -- | Get the required witnesses among the certificates, keeping the required registration 
    -- witnesses separate.
    (normalCertWitness,registrationCertWits) <- partitionEithers . catMaybes <$> 
      sequence (map checkCertificate _certificates)
    
    -- | Remove repeat keys before returning. You cannot have the same `PubKeyHash` with different
    -- `DerivationPath`s so it is enough to just check the key hashes. Filter out registration
    -- certificate keys if they are also normal witnesses.
    return $ 
      ( ordNubOn (fst . view witness) $ inputWitnesses <> normalCertWitness
      , ordNubOn (fst . view witness) $
          filterOutDoubleCertKeys normalCertWitness registrationCertWits
      )
  where
    -- Get the pubkeyhash from the plutus address. Returns nothing if the input is a script input.
    checkInput :: (Int,VerifiedInput) -> Either Text (Maybe NormalWitness)
    checkInput (index,i) = do
      plutusAddr <- 
        first (const $ "Input " <> show index <> " has an invalid bech32 shelley address.") $
          paymentAddressToPlutusAddress $ i ^. paymentAddress
      return $ case toPubKeyHash plutusAddr of
        Nothing -> Nothing
        Just pkHash -> Just $ NormalWitness (pkHash, i ^. paymentKeyPath)

    checkCertificate 
      :: (Int,VerifiedCertificate) 
      -> Either Text (Maybe (Either NormalWitness RegistrationWitness))
    checkCertificate (index,cert) = do
      cred <- 
        first (const $ "Certificate " <> show index <> " has an invalid bech32 stake address.") $
          stakeAddressToPlutusCredential $ cert ^. stakeAddress
      let wit = case cred of
            PubKeyCredential pkh -> Just (pkh, cert ^. stakeKeyPath)
            _ -> Nothing
      case cert ^. certificateAction of
        Registration -> return $ fmap (Right . RegistrationWitness) wit
        _ -> return $ fmap (Left . NormalWitness) wit

    -- If a stake key is registering AND delegating in the same transaction, the key does not
    -- need to be included twice. It can be removed from the registration witnesses.
    filterOutDoubleCertKeys :: [NormalWitness] -> [RegistrationWitness] -> [RegistrationWitness]
    filterOutDoubleCertKeys normalWits regWits =
      flip filter regWits $ \(RegistrationWitness regWit) -> 
        (NormalWitness regWit) `notElem` normalWits

-- | Populate the `VerifiedInput` with the information from the corresponding `AddressUTxO`.
updateVerifiedInput :: AddressUTxO -> VerifiedInput -> VerifiedInput
updateVerifiedInput utxo input =
  input & paymentAddress .~ (utxo ^. paymentAddress)
        & lovelaces .~ (utxo ^. lovelaces)
        & nativeAssets .~ (utxo ^. nativeAssets)

-- | Check the tracked wallets for the target UTxO. If it is found, update the required
-- information. It returns `Nothing` if the input is not found among the tracked wallets.
findUTxOAmongWallets :: VerifiedInput -> Wallets -> Maybe VerifiedInput
findUTxOAmongWallets input@VerifiedInput{_utxoRef} wallets = 
    asum
      [ findAmongPaymentWallets $ wallets ^. paymentWallets
      ]
  where
    findAmongPaymentWallets :: [PaymentWallet] -> Maybe VerifiedInput 
    findAmongPaymentWallets [] = Nothing
    findAmongPaymentWallets (x:xs) =
      case find (\u -> _utxoRef == u ^. utxoRef) $ x ^. utxos of
        Nothing -> findAmongPaymentWallets xs
        Just utxo -> 
          -- Update the normal information and then also update the paymentKeyPath.
          let updatedInput = updateVerifiedInput utxo input
          in Just $ updatedInput & paymentKeyPath .~ (x ^. paymentKeyPath)

-- | Check a user specified input and return the first error that occurs. Otherwise,
-- properly format the new input and add it to the model. If the new input's index is 0,
-- then it is a new input, otherwise it is editing the input with the corresponding index.
-- Rebalance the transaction after updating the model.
processNewInput :: Wallets -> TxBuilderModel -> Either Text TxBuilderModel
processNewInput ws tx@TxBuilderModel{_inputs,_newInput = (index,input)} = do
    verified <- toVerifiedInput input

    let -- If the index is currently zero, the new index needs to be calculated. If the index is
        -- currently not zero, then this index is an edit and the old version must be removed from
        -- the inputs list.
        (newIndex,otherInputs)
          | index == 0 = (length _inputs + 1, _inputs)
          | otherwise = (index, filter ((/=index) . fst) _inputs)

    -- Check if the UTxO is already among the list of inputs. This must be checked after removing
    -- the input with the corresponding index. It doesn't matter what the default value is for 
    -- `Right` here; it will just be dropped. Only the error message is important here.
    void $ first (const "UTxO already among tx inputs.") $
      maybeToLeft True $ find (\(_,i) -> i ^. utxoRef == verified ^. utxoRef) otherInputs

    -- Try to populate the required information from the known UTxOs.
    updatedInput <- case findUTxOAmongWallets verified ws of
      Just updatedInput -> Right updatedInput
      Nothing -> Right verified

    -- Add the new input to the list of inputs, sort them lexicographically, and then re-index
    -- them.
    let newInputs = reIndex $ sortOn (view utxoRef . snd) $ (newIndex, updatedInput) : otherInputs

    -- Set the txFee to 0 (it must be recalculated), rebalance the transaction, and then return it.
    return $ 
      balanceTx $ 
        tx & inputs .~ newInputs
           & txFee .~ 0

-- | Check a user specified change output and return the first error that occurs. Otherwise,
-- properly format the new change output and add it to the model. Rebalance the transaction after 
-- updating the model.
processNewChangeOutput :: Network -> TxBuilderModel -> Either Text TxBuilderModel
processNewChangeOutput network tx@TxBuilderModel{_newChangeOutput = output} = do
  verified <- toVerifiedChangeOutput network output

  -- Replace the old `changeOutput` with the `newChangeOutput`. Set the txFee to 0 
  -- (it must be recalculated), rebalance the transaction, and then return it.
  return $ 
    balanceTx $ 
      tx & changeOutput .~ verified
         & txFee .~ 0

-- | Check a user specified output and return the first error that occurs. Otherwise,
-- properly format the new output and add it to the model. If the new output's index is 0,
-- then it is a new output, otherwise it is editing the output with the corresponding index.
-- Rebalance the transaction after updating the model.
processNewOutput :: Network -> TxBuilderModel -> Either Text TxBuilderModel
processNewOutput network tx@TxBuilderModel{_outputs,_newOutput = (index,output)} = do
    verified <- toVerifiedOutput network output

    let -- If the index is currently zero, the new index needs to be calculated. If the index is
        -- currently not zero, then this output is an edit and the old version must be removed from
        -- the outputs list.
        (newIndex,otherOutputs)
          | index == 0 = (length _outputs + 1, _outputs)
          | otherwise = (index, filter ((/=index) . fst) _outputs)

    -- Add the new output to the list of outputs, set the txFee to 0 (it must be recalculated),
    -- rebalance the transaction, and then return it.
    return $ 
      balanceTx $ 
        tx & outputs .~ sortOn fst ( (newIndex,verified) : otherOutputs )
           & txFee .~ 0

-- | Check a user specified certificate and return the first error that occurs. Otherwise,
-- properly format the new certificate and add it to the model. Rebalance the transaction after 
-- updating the model.
processNewCertificate :: Network -> TxBuilderModel -> Either Text TxBuilderModel
processNewCertificate network tx@TxBuilderModel{_certificates,_newCertificate = (index,cert)} = do
  verified <- toVerifiedCertificate network cert

  let -- If the index is currently zero, the new index needs to be calculated. If the index is
      -- currently not zero, then this certificate is an edit and the old version must be removed 
      -- from the certificates list.
      (newIndex,otherCerts)
        | index == 0 = (length _certificates + 1, _certificates)
        | otherwise = (index, filter ((/=index) . fst) _certificates)

  -- Replace the old `changeOutput` with the `newChangeOutput`. Set the txFee to 0 
  -- (it must be recalculated), rebalance the transaction, and then return it.
  return $ 
    balanceTx $ 
      tx & certificates .~ sortOn fst ( (newIndex,verified) : otherCerts )
         & txFee .~ 0
