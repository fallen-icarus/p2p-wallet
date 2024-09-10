module P2PWallet.GUI.HelpMessages where

import P2PWallet.Prelude

accountIdInfoMsg :: Text
accountIdInfoMsg = unlines
  [ "Hardware wallet key derivation paths take the form:"
  , "m / purpose' / coin_type' / account' / chain / address_index "
  , ""
  , "For example, the following path is for the first payment key from account 0:"
  , "1852'/1815'/0'/0/0"
  , ""
  , "This example is for the second stake key from account 0:"
  , "1852'/1815'/0'/2/1"
  , ""
  , unwords
      [ "All keys used for a given profile MUST be from the same account."
      , "This requirement is because hardware wallets only allow signing transactions"
      , "when all of its required keys are from the same account. It is technically possible"
      , "to sign with keys from other account indices, but each account index must individually"
      , "witness the transaction. In practice, this means the user must review the transaction"
      , "once for each account index required. This would make for a bad user experience, and"
      , "also makes the signing logic more annoying to write... Since users can create as many"
      , "payment keys and stake keys as they wish for each account index, there is no real reason"
      , "to enable supporting multiple account indices for each profile."
      ]
  , ""
  , unwords
      [ "The p2p-wallet will use this account index when pairing any hardware wallet keys with"
      , "this profile."
      ]
  , ""
  , unwords
      [ "NOTE: Derivation paths can also be written with 'H' instead of '''; they are equivalent."
      , "For example, the path for the first payment key from account 0 can also be written as:"
      ]
  , "1852H/1815H/0H/0/0"
  , ""
  , unwords
      [ "Since the single quote sometimes has a special meaning inside programming languages, the"
      , "'H' is occasionally used instead."
      ]
  ]

paymentAddressIndexMsg :: Text
paymentAddressIndexMsg = unlines
  [ "Hardware wallet key derivation paths take the form:"
  , "m / purpose' / coin_type' / account' / chain / address_index "
  , ""
  , unwords
      [ "All payment keys have 'chain' set to 0, but the 'address_index' can technically"
      , "be any whole number >= 0."
      ]
  , ""
  , "For example, the following path is for the first payment key from account 0:"
  , "1852'/1815'/0'/0/0"
  , ""
  , "This example is for the second payment key from account 0:"
  , "1852'/1815'/0'/0/1"
  , ""
  , unwords
      [ "You have full control over which payment key to use for each payment address."
      , "To keep things simple, it is recommended to go in order; jumping around with the"
      , "'address_index' will make it harder for you to remember which indices you are using."
      , "You will always be able to recover the address as long as you remember which 'account'"
      , "and 'address_index' were used in the payment key's deriviation path."
      ]
  , ""
  , unwords
      [ "The p2p-wallet will use the account index set for this profile"
      , "when pairing the new hardware wallet keys."
      ]
  , ""
  , unwords
      [ "NOTE: Derivation paths can also be written with 'H' instead of '''; they are equivalent."
      , "For example, the path for the first payment key from account 0 can also be written as:"
      ]
  , "1852H/1815H/0H/0/0"
  , ""
  , unwords
      [ "Since the single quote sometimes has a special meaning inside programming languages, the"
      , "'H' is occasionally used instead."
      ]
  ]

stakeAddressIndexMsg :: Text
stakeAddressIndexMsg = unlines
  [ "Hardware wallet key derivation paths take the form:"
  , "m / purpose' / coin_type' / account' / chain / address_index "
  , ""
  , unwords
      [ "All stake keys have 'chain' set to 2, but the 'address_index' can technically"
      , "be any whole number >= 0."
      ]
  , ""
  , "For example, the following path is for the first stake key from account 0:"
  , "1852'/1815'/0'/2/0"
  , ""
  , "This example is for the second stake key from account 0:"
  , "1852'/1815'/0'/2/1"
  , ""
  , unwords
      [ "You have full control over which stake key to use for each stake address."
      , "To keep things simple, it is recommended to go in order; jumping around with the"
      , "'address_index' will make it harder for you to remember which indices you are using."
      , "You will always be able to recover the address as long as you remember which 'account'"
      , "and 'address_index' were used in the stake key's deriviation path."
      ]
  , ""
  , unwords
      [ "The p2p-wallet will use the account index set for this profile"
      , "when pairing the new hardware wallet keys."
      ]
  , ""
  , unwords
      [ "NOTE: Derivation paths can also be written with 'H' instead of '''; they are equivalent."
      , "For example, the path for the first payment key from account 0 can also be written as:"
      ]
  , "1852H/1815H/0H/0/0"
  , ""
  , unwords
      [ "Since the single quote sometimes has a special meaning inside programming languages, the"
      , "'H' is occasionally used instead."
      ]
  ]

whatIsPairedWalletMsg :: Text
whatIsPairedWalletMsg = unwords
  [ "A paired wallet is a hardware wallet address where the p2p-wallet knows about the associated"
  , "hardware wallet keys. Because it is aware of the keys, users can sign/witness any transaction"
  , "involving the paired wallet. The p2p-wallet only supports signing/witnessing transactions"
  , "for paired wallets!"
  ]

whatIsWatchedWalletMsg :: Text
whatIsWatchedWalletMsg = unlines
  [ unwords
      [ "A watched wallet is an address where the p2p-wallet does NOT know about the associated"
      , "credentials. For example, if the user has a cold wallet where the keys are on an air-gapped"
      , "computer, there is no possible way for the p2p-wallet to safely automate the"
      , "signing/witnessing of transactions involving this address. However, the p2p-wallet can still"
      , "be used to build and submit transactions for these cold wallets. You can think of watched"
      , "wallets as windows into those addresses. Since the p2p-wallet can see the UTxOs located at"
      , "watched addresses, the transaction builder is able to use UTxOs from these addresses."
      ]
  , ""
  , unwords
      [ "Any transaction built for watched wallets can be exported from the p2p-wallet so that it"
      , "can be signed externally. Once signed, the completed transaction can be imported back"
      , "into the p2p-wallet for submission to the blockchain. If a paired wallet must also"
      , "witness the transaction, the p2p-wallet can have the hardware wallet keys witness the"
      , "transaction before exporting both the tx.body file and tx.witness file for the keys. All"
      , "witness files must be aggregated externally; the p2p-wallet only supports importing"
      , "tx.signed files for submission to the blockchain."
      ]
  ]

utxoSearchMsg :: Text
utxoSearchMsg = unlines
  [ unwords
      [ "You can search through all of the UTxOs for specific native tokens, reference script"
      , "hashes, datum hashes, or tx hashes. You can even search for UTxOs containing a"
      , "combination of targets by separating them with commas."
      ]
  , ""
  , "Native asset criteria must be one of:"
  , "1. policy_id.asset_name"
  , "2. policy_id"
  , "3. asset_name"
  , "4. fingerprint"
  , "5. ticker"
  ]

utxoSortMsg :: Text
utxoSortMsg = unlines
  [ "UTxOs can be sorted based off one of the following methods:"
  , "1. Lexicographically based on the UTxO's output reference"
  , "2. Based on the balance of Ada"
  , "3. Chronologically based on the time the UTxO was created"
  , "4. Based on the balance of a specific native asset"
  , ""
  , unwords
      [ "The last option is only possible when search is being used for the UTxOs. This option"
      , "will be hidden if search is not being used. The asset that will be sorted on will"
      , "be the FIRST criteria used in the search. Make sure the first"
      , "criteria is actually an asset... To sort based off of a different native asset, move that"
      , "asset so that it is first in the search criteria."
      ]
  ]

depositSignMsg :: Text
depositSignMsg = unlines
  [ unwords
      [ "Positive means the deposit was paid. Negative means the deposit was reclaimed."
      , "When stake registration deposits are paid/reclaimed, they will appear here."
      ]
  ]

homeTxSearchMsg :: Text
homeTxSearchMsg = unlines
  [ "You can search through all of the transactions for any of the following criteria:"
  , "1. Native assets"
  , "2. Payment addresses"
  , "3. Stake addresses"
  , "4. Reference script hashes"
  , "5. Datum hashes"
  , "6. UTxO output reference"
  , ""
  , "You can even use a combination of criteria by separating them with commas."
  , ""
  , "Native asset criteria must be one of:"
  , "1. policy_id.asset_name"
  , "2. policy_id"
  , "3. asset_name"
  , "4. fingerprint"
  , "5. ticker - if in Ticker Registry"
  , ""
  , "The search will try to match against any of:"
  , "1. Inputs"
  , "2. Outputs"
  , "3. Reference inputs"
  , "4. Reward withdrawals"
  , "5. Certificates"
  ]

txStartDateMsg :: Text
txStartDateMsg = unlines
  [ unwords
      [ "Show only transactions after the specfied date. Leave it blank to show transactions since"
      , "the beginning. The default setting is to only show transactions within the past 30 days."
      ]
  ]

txEndDateMsg :: Text
txEndDateMsg = unlines
  [ unwords
      [ "Show only transactions up through the specfied date. Leave it blank to show transactions"
      , "up through the present."
      ]
  ]

totalDelegatedMsg :: Text
totalDelegatedMsg = unlines
  [ unwords
      [ "The total amount delegated across all payment addresses using this staking credential +"
      , "the current reward balance."
      ]
  ]

pledgeMsg :: Text
pledgeMsg = unlines
  [ unwords
      [ "The amount of ADA the stake pool owners have committed as pledge. If this is not met,"
      , "the stake pool will be penalized."
      ]
  ]

activePledgeMsg :: Text
activePledgeMsg = unlines
  [ unwords
      [ "The actual amount of ADA pledged by the stake pool owners. It must be at the pledge"
      , "amount to avoid penalties."
      ]
  ]

fixedCostMsg :: Text
fixedCostMsg = unlines
  [ unwords
      [ "A fixed amount of ADA deducted from the total rewards to cover the stake pool's"
      , "operating expenses."
      ]
  ]

marginMsg :: Text
marginMsg = unlines
  [ unwords
      [ "The percentage of the pool's total rewards that will be taken by the stake pool operator"
      , "after the fixed cost."
      ]
  ]

liveSaturationMsg :: Text
liveSaturationMsg = unlines
  [ unwords
      [ "The percent saturation for this pool."
      ]
  ]

activeLinkedAddressesMsg :: Text
activeLinkedAddressesMsg = unlines
  [ unwords
      [ "Payment addresses with non-zero balances that are using this staking credential."
      , "This includes any DeFi addresses using this staking credential."
      ]
  ]

spendableEpochMsg :: Text
spendableEpochMsg = unlines
  [ unwords
      [ "The epoch when these rewards became spendable."
      ]
  ]

nativeAssetAreaEntryMsg :: Text
nativeAssetAreaEntryMsg = unlines
  [ "Native assets must be in one of the following formats:"
  , "1. '# policy_id.asset_name'"
  , "2. '# fingerprint'"
  , "3. '# ticker' - if in Ticker Registry"
  , ""
  , unwords
      [ "You can go to another page and copy the name; then return here to paste it in."
      , "Your place will be saved."
      ]
  , ""
  , unwords
      [ "The first two formats must always be paired with a whole number (the fingerprint is just"
      , "an alias for 'policy_id.asset_name'), but the third format can be paired with a decimal."
      ]
  , ""
  , unwords
      [ "If you use the ticker, be sure to account for the decimal places! For example, if the"
      , "fingerprint is 'abc123', the ticker is `ABC`, and the asset uses 6 decimal places, then"
      , "'1 abc123' is NOT the same as '1 ABC'! Instead, '1 ABC' would be '1000000 abc123'! Pay"
      , "attention to the conversions!"
      ]
  ]

testTokenMintQuantitiesMsg :: Text
testTokenMintQuantitiesMsg = unlines
  [ unwords 
      [ "Test token names must be in the format '# asset_name' where 'asset_name' is in"
      , "hexidecimal. You can use the provided converter to convert human-readable text"
      , "to hexidecimal."
      ]
  , ""
  , "Negative quantities imply burning while positive quantities imply minting."
  ]

aboutCollateralMsg :: Text
aboutCollateralMsg = unlines
  [ "Only transactions with smart contract executions require collateral."
  , ""
  , unwords 
      [ "Collateral UTxOs are NOT spent unless a smart contract execution fails."
      , "This should never actually happen for any of the protocols supported by this wallet."
      , "If someone beats you to a DeFi UTxO, the transaction will fail due to the UTxO no longer"
      , "being present; no smart contracts will be executed in this scenario."
      , "However, even if the collateral is never taken, it must still be included with each"
      , "transaction."
      ]
  , ""
  , unwords 
      [ "Collateral UTxOs must contain at least 5 ADA and not have any native assets present."
      , "Only one collateral UTxO is required per transaction with smart contract executions."
      ]
  , ""
  , unwords 
      [ "The actual amount of ada required for collateral will be calculated while building"
      , "the transaction. After subtracting the required collateral amount from the 5 ADA,"
      , "the remainder will be returned to the address the collateral input belongs to. This"
      , "collateral change output will only be created if the collateral is actually taken!"
      , "If no collateral is taken (due to the smart contracts successfully executing), the"
      , "collateral input UTxO will remain un-touched."
      ]
  ]

defiStakeCredentialMsg :: Text
defiStakeCredentialMsg = unlines
  [ unwords 
      [ "All p2p-DeFi DApps allow you to use your own staking credential to protect your DeFi"
      , "assets. This enables you to maintain custody, delegation control, and voting control"
      , "of all assets currently being used for DeFi."
      ]
  , ""
  , unwords 
      [ "Please choose one of your tracked stake wallets to use for the stake credential."
      , "If you wish to use a different one, you must first add that stake wallet from the"
      , "`Staking` page."
      ]
  ]

offerAssetMsg :: Text
offerAssetMsg = unlines
  [ "Which asset do you currently have and wish to convert?"
  , ""
  , "The asset name must be in one of the following formats:"
  , "1. 'policy_id.asset_name'"
  , "2. 'ticker' - if in Ticker Registry"
  , "3. 'ADA' - if the asset is ada."
  , ""
  , unwords
      [ "Fingerprints are not supported because there is no way for the p2p-wallet to know"
      , "which asset the fingerprint corresponds to unless it knows the asset in advance."
      , "Due to the composability of these DeFi protocols, users may not always have the offer asset"
      , "in question."
      ]
  ]

askAssetMsg :: Text
askAssetMsg = unlines
  [ "Which asset do you wish to receive?"
  , ""
  , "The asset name must be in one of the following formats:"
  , "1. 'policy_id.asset_name'"
  , "2. 'ticker' - if in Ticker Registry"
  , "3. 'ADA' - if the asset is ada."
  , ""
  , unwords
      [ "Fingerprints are not supported because there is no way for the p2p-wallet to know"
      , "which asset the fingerprint corresponds to unless it knows the asset in advance."
      , "This may not be realistic in most scenarios since users don't typically have the"
      , "asset they wish to receive."
      ]
  ]

p2pLimitOrderMsg :: Text
p2pLimitOrderMsg = unlines
  [ unwords
      [ "A p2p limit order is a one-directional swap where users can take some of your offer"
      , "asset as long as they deposit enough of the ask asset to satisfy the limit price."
      , "Anyone can directly interact with your swap without needing to go through batchers."
      ]
  ]

limitPositionSizeMsg :: Text
limitPositionSizeMsg = unlines
  [ unwords
      [ "How much of the offer asset are you going to deposit into the swap?"
      , "All swaps must be stored with the minimum UTxO value amount of ADA in addition"
      , "to the offer amount! The minimum UTxO value is effectively a deposit you can"
      , "get back by closing the swap."
      ]
  ]

limitPriceMsg :: Text
limitPriceMsg = unlines
  [ unwords
      [ "Anyone who wishes to swap with your assets must satisfy the price you set here."
      , "The price must be specified as a decimal. Pay attention to the required units!"
      ]
  ]

swapArbitrageFeeMsg :: Text
swapArbitrageFeeMsg = unlines
  [ unwords
      [ "Directly interacting with swaps carries a risk that another user will beat you to your"
      , "desired swap before your transaction goes through. This would mean you must keep trying"
      , "different swaps until you find one that goes through. This can get quite annoying..."
      ]
  , ""
  , unwords
      [ "Alternatively, you can pay an arbitrager to take on this role for you: you specify a"
      , "limit order price and the arbitrager will keep trying to satisfy your swap until it goes"
      , "through."
      ]
  , ""
  , unwords
      [ "You have full control over how much to pay arbitragers to take on the concurrency risk"
      , "on your behalf. It is NOT possible for arbitragers to cheat you! The more you are willing"
      , "to pay, the faster your swap is likely to get processed. If you do not want to pay"
      , "arbitragers a fee, you can set the 'Arbitrage Fee' to '0%' or leave the field blank."
      ]
  , ""
  , unwords
      [ "WARNING: In the early days of p2p-DeFi, there may not be many arbitragers and so your swaps"
      , "may still sit for a while, even with an 'Arbitrage Fee' set. This is just growing pains..."
      , "However, this also means there may be very little concurrency risk in the early days and"
      , "you may be able to directly interact with swaps without much of a problem."
      ]
  , ""
  , "The formula for the actual price used in your swap is:"
  , "limit_price * (1 - arbitrage_fee_as_decimal)"
  ]

p2pLiquiditySwapMsg :: Text
p2pLiquiditySwapMsg = unlines
  [ unwords
      [ "A p2p liquidity swap is a two-directional swap where users can swap with your assets"
      , "in either direction. Since you can specify a separate price for each direction,"
      , "you can charge a fee for each conversion using your liquidity swap. The liquidity swap"
      , "can be endlessly converted back and forth. The prices can be used to favor one direction"
      , "over the other."
      ]
  ]

liquidityPositionSizeMsg :: Text
liquidityPositionSizeMsg = unlines
  [ unwords
      [ "How much of this asset are you going to deposit into the liquidity swap?"
      , "All swaps must be stored with at the minimum UTxO value of ADA in addition to the"
      , "liquidity amount! The minimum UTxO value is effectively a deposit you can get back"
      , "by closing the swap."
      ]
  ]

liquidityPriceMsg :: Text
liquidityPriceMsg = unlines
  [ "The price must be specified as a decimal. Pay attention to the required units!"
  ]

cardanoSwapsBugMsg :: (Show a) => a -> Text
cardanoSwapsBugMsg a = unlines
  [ unwords
      [ "You found a critical bug in cardano-swaps! Beacons were found with an invalid swap UTxO."
      , "This bug can be used to denial-of-service attack the order book!"
      ]
  , ""
  , unwords
      [ "Your funds are still safe; no one can swap your assets for less than the amount you asked for."
      , "This bug just means it may be hard for other users to find your swaps."
      ]
  , ""
  , unwords 
      [ "Please open an issue on github explaining how you came across the bug and please attach"
      , "the following information (you can copy/paste it into the new issue):"
      ]
  , ""
  , show a
  ]

offerAssetPositionFilterMsg :: Text
offerAssetPositionFilterMsg = unlines
  [ "Filter open positions for those selling a specific asset."
  , ""
  , "The asset name must be in one of the following formats:"
  , "1. 'policy_id.asset_name'"
  , "2. 'ticker' - if in Ticker Registry"
  , "3. 'ADA' - if the asset is ada."
  , ""
  , "Fingerprints are not supported."
  ]

askAssetPositionFilterMsg :: Text
askAssetPositionFilterMsg = unlines
  [ "Filter open positions for those buying a specific asset."
  , ""
  , "The asset name must be in one of the following formats:"
  , "1. 'policy_id.asset_name'"
  , "2. 'ticker' - if in Ticker Registry"
  , "3. 'ADA' - if the asset is ada."
  , ""
  , "Fingerprints are not supported."
  ]

positionsSortMsg :: Text
positionsSortMsg = unlines
  [ "Open positions can be sorted based off one of the following methods:"
  , "1. Lexicographically based on the UTxO's output reference"
  , "2. Chronologically based on the time the UTxO was created"
  , "3. Based on the balance of Offer Asset in the UTxO"
  , "4. Based on the balance of Ask Asset in the UTxO"
  , "5. Based on the limit price"
  , ""
  , unwords
      [ "Options 3 and 4 are only available when the offer asset and/or the ask asset are set in"
      , "the filter settings."
      ]
  , ""
  , unwords
      [ "Option 5 is only available when both the offer asset and the ask asset are set in"
      , "the filter settings. It will sort based on the limit price for that swap direction."
      ]
  ]

purchaseAmountMsg :: Text -> Text
purchaseAmountMsg assetName = unlines
  [ "How many units of " <> assetName <> " would you like to purchase?"
  ]

offerAssetTxFilterMsg :: Text
offerAssetTxFilterMsg = unlines
  [ "Filter transactions for those involving swaps that are selling a specific asset."
  , ""
  , "The asset name must be in one of the following formats:"
  , "1. 'policy_id.asset_name'"
  , "2. 'ticker' - if in Ticker Registry"
  , "3. 'ADA' - if the asset is ada."
  , ""
  , "Fingerprints are not supported."
  ]

askAssetTxFilterMsg :: Text
askAssetTxFilterMsg = unlines
  [ "Filter transactions for those involving swaps that are buying a specific asset."
  , ""
  , "The asset name must be in one of the following formats:"
  , "1. 'policy_id.asset_name'"
  , "2. 'ticker' - if in Ticker Registry"
  , "3. 'ADA' - if the asset is ada."
  , ""
  , "Fingerprints are not supported."
  ]

fullyConvertedSwapMsg :: Text
fullyConvertedSwapMsg = unlines
  [ "Does the swap have any of the offer asset left to trade?"
  , ""
  , unwords
      [ "When ada is being sold, the swap will be considered fully converted if there is less"
      , "than 5 ADA left in the swap. Since all swaps must contain at least some ada, a swap"
      , "with less than 5 ADA available to purchase may not have enough to satisfy the purchase"
      , "AND still leave ada left over to satisfy the blockchain's minUTxOValue requirement."
      ]
  ]

trezorDerivationMsg :: Text
trezorDerivationMsg = unlines
  [ "Which derivation method would you like to use?"
  , ""
  , unwords
      [ "WARNING: It is not recommended to create new wallets using the 'Trezor-Icarus' method."
      , "It is only included to enable recovery of old wallets. If you are still using this"
      , "derivation method for your trezor wallet, it is advised that you migrate your funds"
      , "to a wallet using the 'Icarus' derivation method."
      ]
  ]

directoryPathMsg :: Text
directoryPathMsg = unlines
  [ unwords
      [ "The absolute path to the target directory. Environment variables and '~' (for the home"
      , "directory) are supported."
      ]
  , ""
  , "On POSIX systems, environment variables must begin with '$'. Example entries:"
  , "1. $HOME/Documents/"
  , "2. $CARDANO_NODE_SOCKET_PATH/"
  , "3. ~/$PERSONAL_FILES/"
  , ""
  , "On Windows systems, environment variables must be surrounded by '%'. Example entries:"
  , "1. %APPDATA%\\Documents\\"
  , "2. %HOMEPATH%\\"
  , "3. ~\\Documents\\"
  ]

filePathMsg :: Text
filePathMsg = unlines
  [ unwords
      [ "The absolute path to the target file. Environment variables and '~' (for the home"
      , "directory) are supported."
      ]
  , ""
  , "On POSIX systems, environment variables must begin with '$'. Example entries:"
  , "1. $HOME/Documents/tx.signed"
  , "2. $CARDANO_NODE_SOCKET_PATH/tx.signed"
  , "3. ~/$PERSONAL_FILES/tx.signed"
  , ""
  , "On Windows systems, environment variables must be surrounded by '%'. Example entries:"
  , "1. %APPDATA%\\Documents\\tx.signed"
  , "2. %HOMEPATH%\\tx.signed"
  , "3. ~\\Documents\\tx.signed"
  ]

askCollateralMsg :: Text
askCollateralMsg = unlines
  [ unwords
      [ "Which assets are you willing to use for collateral? Not all assets you list here must be"
      , "used when you actually accept an offer."
      ]
  , ""
  , "The asset name must be in one of the following formats:"
  , "1. 'policy_id.asset_name'"
  , "2. 'ticker' - if in Ticker Registry"
  , "3. 'ADA' - if the asset is ada."
  , ""
  , "Fingerprints are not supported."
  , ""
  , "The Ask UTxO must be stored with at least one unit of each asset you list here."
  ]

askLoanAmountMsg :: Text
askLoanAmountMsg = unlines
  [ "Which asset, and how, much are you looking to borrow?"
  , ""
  , "The asset name must be in one of the following formats:"
  , "1. '# policy_id.asset_name'"
  , "2. '# ticker' - if in Ticker Registry"
  , "3. '# ADA' - if the asset is ada."
  , ""
  , "Fingerprints are not supported."
  ]

askDurationMsg :: Text
askDurationMsg = unlines
  [ unwords
      [ "How long would you like the loan to last for? This must be specified as a number of days."
      , "The total time will be 86400000 milliseconds * number of days."
      ]
  ]

askCfgLoanAmountMsg :: Text
askCfgLoanAmountMsg = unlines
  [ "Which asset are you interested in for the loan asset?"
  , ""
  , "The asset name must be in one of the following formats:"
  , "1. 'policy_id.asset_name'"
  , "2. 'ticker' - if in Ticker Registry"
  , "3. 'ADA' - if the asset is ada."
  , ""
  , "Fingerprints are not supported."
  ]

askCfgMinDurationMsg :: Text
askCfgMinDurationMsg = unlines
  [ "What is the minimum loan duration (in whole number of days) you are interested in?"
  ]

askCfgMaxDurationMsg :: Text
askCfgMaxDurationMsg = unlines
  [ "What is the maximum loan duration (in whole number of days) you are interested in?"
  ]

askCfgCollateralMsg :: Text
askCfgCollateralMsg = unlines
  [ "Which assets are you interested in being used as collateral?"
  , ""
  , "The asset name must be in one of the following formats:"
  , "1. 'policy_id.asset_name'"
  , "2. 'ticker' - if in Ticker Registry"
  , "3. 'ADA' - if the asset is ada."
  , ""
  , "Fingerprints are not supported."
  ]

openAsksSortMsg :: Text
openAsksSortMsg = unlines
  [ "Open asks can be sorted based off one of the following methods:"
  , "1. Lexicographically based on the UTxO's output reference"
  , "2. Chronologically based on the time the UTxO was created"
  , "3. Based on the loan amount"
  , "4. Based on the duration of the requested loan"
  ]

offerBorrowerIdMsg :: Text
offerBorrowerIdMsg = unwords
  [ "The borrower id is the borrower's staking credential. The credit history for this borrower is"
  , "tied to this identifier."
  ]

offerLoanAmountMsg :: Text
offerLoanAmountMsg = unlines
  [ "How much will you lend?"
  , ""
  , "The asset quantity must be in one of the following formats:"
  , "1. '# policy_id.asset_name'"
  , "2. '# ticker' - if in Ticker Registry"
  , "3. '# ADA' - if the asset is ada."
  , ""
  , "Fingerprints are not supported."
  ]

offerPaymentAddressMsg :: Text
offerPaymentAddressMsg = unlines
  [ "The address the borrower will make loan payments to."
  , ""
  , unwords
      [ "When the offer is accepted, the borrower will deposit the Key NFT for the new loan to"
      , "this address. This Key NFT can be used to update the payment address during the loan,"
      , "and it can be freely traded on a secondary market."
      ]
  ]

offerInterestMsg :: Text
offerInterestMsg = unlines
  [ "The interest that will be applied at the end of each compound period."
  , ""
  , "The first interest application is applied immediately upon accepting the offer."
  , ""
  , "To offer a non-compounding interest loan, leave the compound frequency field blank."
  ]


offerCompoundFrequencyMsg :: Text
offerCompoundFrequencyMsg = unlines
  [ unwords
      [ "How often the interest will be applied, and how long the borrower has to satisfy the"
      , "minimum payment requirement."
      ]
  , ""
  , "It must be specified in # of days. Leave the field blank to not set a compounding frequency."
  , ""
  , unwords
      [ "If you do not set a compounding period, you will not be able to set a minimum payment and"
      , "penalty."
      ]
  ]

offerMinimumPaymentMsg :: Text
offerMinimumPaymentMsg = unlines
  [ unwords
      [ "How much the borrower must pay back each compounding period. This is similar to the"
      , "required monthly payment of TradFi loans."
      ]
  , ""
  , "It must be in units of the loan asset."
  ]

offerPenaltyMsg :: Text
offerPenaltyMsg = unlines
  [ "Penalties can only be set when there is a required minimum payment greater than 0."
  , ""
  , unwords
      [ "The penalty is used to incentivize the borrower to make regular loan payments."
      , "There are three possible penalties:"
      ]
  , "1. No Penalty"
  , unwords
      [ "2. Fixed Fee Penalty - the outstanding loan balance will increase by a fixed amount every time"
      , "the minimum payment is not met in a given compounding period."
      ]
  , unwords
      [ "3. Percent Penalty - the outstanding loan balance will increase by a percentage every time"
      , "the minimum payment is not met in a given compounding period."
      ]
  ]

offerCollateralIsSwappableMsg :: Text
offerCollateralIsSwappableMsg = unlines
  [ "Whether the borrower can swap out collateral for other approved collateral during the loan."
  , ""
  , unwords
      [ "For example, if the loan allows for ADA and AGIX to be used as collateral, but the loan"
      , "is currently only backed by ADA, the borrower can withdraw some ADA during a loan payment"
      , "as long as they also deposit enough AGIX so that the relative value of the collateral"
      , "matches the required amount."
      ]
  , ""
  , unwords
      [ "The borrower cannot do this without consent from you (the lender). This option can be used"
      , "to make your offer more competitive than offers from other lenders. This setting cannot be"
      , "changed during the loan."
      ]
  ]

offerClaimPeriodMsg :: Text
offerClaimPeriodMsg = unlines
  [ "How long do you need to claim the collateral after the borrower defaults?"
  , ""
  , unwords
      [ "In order to prevent permanently locked collateral, the DApp will consider the collateral"
      , "\"Lost\" after the specified amount of time has passed. After which, the borrower can"
      , "reclaim the collateral themselves (this will still count as a default)."
      ]
  , ""
  , "The time must be specified in number of days."
  ]

offerLoanTermMsg :: Text
offerLoanTermMsg = unlines
  [ "How long will the loan be active for?"
  , ""
  , unwords
      [ "You can create an offer for a different duration than what the borrower asked for."
      , "This can be considered a counter-offer and is a natural part of negotiations."
      ]
  , ""
  , "The time must be specified in number of days."
  ]

offerExpirationMsg :: Text
offerExpirationMsg = unlines
  [ "How long do you want this offer to be valid for?"
  , ""
  , unwords
      [ "Once the specified amount of time has passed, the borrower will not be able to accept"
      , "the offer."
      ]
  , ""
  , "You can leave this field blank to not set an expiration."
  ]

offerCollateralMsg :: Text
offerCollateralMsg = unlines
  [ "What are the relative values of each collateral asset?"
  , ""
  , unwords
      [ "The pairings must be separated with newlines and each pairing must be in the"
      , "format of 'collateral, price'. The prices must be in units of:"
      ]
  , "collateral_asset / loan_asset"
  , ""
  , unwords
      [ "The DApp will use these prices to determine if the borrower has put up enough collateral"
      , "for the loan. If you do not want a particular asset used as collateral, you can either"
      , "remove the asset from the list or set the price to 0. You can also add an alternative"
      , "collateral asset as a suggestion to the borrower; the borrower can choose whether to"
      , "use the new collateral asset instead of their own suggestions."
      ]
  , ""
  , unwords
      [ "These prices cannot be changed during the loan!!! If you think the value of the collateral"
      , "will change during the life of the loan, make sure to have your prices reflect this"
      , "possiblity! If the loan ever becomes under-collateralized, this is your fault! The borrower"
      , "will not be required to put up more collateral after the fact."
      ]
  ]

collateralAmountsMsg :: Text
collateralAmountsMsg = unlines
  [ "How much of each collateral will you use?"
  , ""
  , "The asset quantities must be in one of the following formats:"
  , "1. '# policy_id.asset_name'"
  , "2. '# ticker' - if in Ticker Registry"
  , "3. '# ADA' - if the asset is ada."
  , ""
  , "Fingerprints are not supported."
  , ""
  , unwords
      [ "WARNING: All Cardano UTxOs require a minimum amount of ada. If ada can be used as collateral,"
      , "it must be used so that the minimum ada deposit will count towards the collateral."
      , "You will not be able to fully reclaim the ada until after the loan is fully paid off! It does"
      , "not matter if there are still other assets left as collateral; the collateral UTxO must"
      , "always contain ada."
      ]
  ]

paymentCollateralAmountsMsg :: Text
paymentCollateralAmountsMsg = unlines
  [ unwords
      [ "You can reclaim collateral proportionally to how much you are paying back."
      , "You must specify how much collateral you intend to leave locked up."
      ]
  , ""
  , "The asset quantities must be in one of the following formats:"
  , "1. '# policy_id.asset_name'"
  , "2. '# ticker' - if in Ticker Registry"
  , "3. '# ADA' - if the asset is ada."
  , ""
  , "Fingerprints are not supported."
  , ""
  , unwords
      [ "WARNING: All Cardano UTxOs require a minimum amount of ada. If ada can be used as collateral,"
      , "it must be used so that the minimum ada deposit will count towards the collateral."
      , "You will not be able to fully reclaim the ada until after the loan is fully paid off! It does"
      , "not matter if there are still other assets left as collateral or if the collateral is"
      , "technically \"unlocked\" by cardano-loans; the collateral UTxO must always contain ada."
      ]
  ]

paymentAmountMsg :: Text
paymentAmountMsg = unlines
  [ "How much will you pay back?"
  , ""
  , "The asset quantities must be in one of the following formats:"
  , "1. '# policy_id.asset_name'"
  , "2. '# ticker' - if in Ticker Registry"
  , "3. '# ADA' - if the asset is ada."
  , ""
  , "Fingerprints are not supported."
  , ""
  , unwords
      [ "The payment output must also have enough ada to satisfy the minUTxOValue. If the loan asset"
      , "is ada, the minUTxOValue is covered by the payment itself. However, if the loan asset is"
      , "a native asset, ada will need to be given to the lender in addition to the native asset"
      , "quantity. This extra ada can be minimized by making the fewest number of payments required"
      , "by the loan."
      ]
  ]

creditScoreMsg :: Text
creditScoreMsg = unlines
  [ "The credit score is calculated by:"
  , "number of loans successfully repaid / number of loans"
  , ""
  , unwords
      [ "Current active loans are not included in this calculation, and an undefined"
      , "score means this credential has not finished any loans yet."
      ]
  ]

lenderOffersSortMsg :: Text
lenderOffersSortMsg = unlines
  [ "Loan offers can be sorted based off one of the following methods:"
  , "1. Lexicographically based on the UTxO's output reference"
  , "2. Based on the offered loan amount"
  , "3. Based on the loan's duration"
  , "4. Chronologically based on the time the offer was created"
  , "5. Based on the loan's interest rate"
  ]

activeLoansSortMsg :: Text
activeLoansSortMsg = unlines
  [ "Active loans can be sorted based off one of the following methods:"
  , "1. Lexicographically based on the UTxO's output reference"
  , "2. Based on the outstanding loan balance"
  , "3. Chronologically based on the time the UTxO was created"
  , "4. Based on the loan's interest rate"
  , "5. Based on the loan's next payment deadline"
  , "6. Based on the loan's amount still owed before the next deadline"
  ]

filterOfferTxByLoanAssetMsg :: Text
filterOfferTxByLoanAssetMsg = unlines
  [ "Show only transactions with offers for the specified loan asset."
  , ""
  , "The asset name must be in one of the following formats:"
  , "1. 'policy_id.asset_name'"
  , "2. 'ticker' - if in Ticker Registry"
  , "3. 'ADA' - if the asset is ada."
  , ""
  , "Fingerprints are not supported."
  ]

filterOfferTxByCollateralMsg :: Text
filterOfferTxByCollateralMsg = unlines
  [ unwords
      [ "Show only transactions with offers using the specified collateral asset."
      , "The names must be separated by newlines."
      ]
  , ""
  , "The asset name must be in one of the following formats:"
  , "1. 'policy_id.asset_name'"
  , "2. 'ticker' - if in Ticker Registry"
  , "3. 'ADA' - if the asset is ada."
  , ""
  , "Fingerprints are not supported."
  ]

activeLoansResearchSortMsg :: Text
activeLoansResearchSortMsg = unlines
  [ "Active loans can be sorted based off one of the following methods:"
  , "1. Lexicographically based on the UTxO's output reference"
  , "2. Based on the outstanding loan balance"
  , "3. Based on the loan's duration"
  , "4. Chronologically based on the time the UTxO was created"
  , "5. Based on the loan's interest rate"
  ]

proposalOfferAmountMsg :: Text
proposalOfferAmountMsg = unlines
  [ "What asset will you give upon contract execution?"
  , ""
  , "The asset quantity must be in one of the following formats:"
  , "1. '# policy_id.asset_name'"
  , "2. '# ticker' - if in Ticker Registry"
  , "3. '# ADA' - if the asset is ada."
  , ""
  , "Fingerprints are not supported."
  ]

proposalAskAssetMsg :: Text
proposalAskAssetMsg = unlines
  [ "What asset do you want upon contract execution?"
  , ""
  , "The asset quantity must be in one of the following formats:"
  , "1. 'policy_id.asset_name'"
  , "2. 'ticker' - if in Ticker Registry"
  , "3. 'ADA' - if the asset is ada."
  , ""
  , "Fingerprints are not supported."
  ]

proposalPremiumAssetMsg :: Text
proposalPremiumAssetMsg = unlines
  [ "What asset do you want the premium to be paid in?"
  , ""
  , "The asset quantity must be in one of the following formats:"
  , "1. 'policy_id.asset_name'"
  , "2. 'ticker' - if in Ticker Registry"
  , "3. 'ADA' - if the asset is ada."
  , ""
  , "Fingerprints are not supported."
  ]

proposalTermsMsg :: Text
proposalTermsMsg = unlines
  [ "What terms are you offering?"
  , ""
  , "The terms must be in the following format:"
  , "premium, strike price, expiration (mm/dd/yy)"
  , ""
  , "Do not specify units! Here is an example:"
  , "10.000000, 2.1, 12/02/24"
  , ""
  , "The premium is the amount of the premium asset to be paid upon purchase of the proposal."
  , ""
  , unwords
      [ "The strike price is the ratio of the ask asset that must be paid in order to take"
      , "the offer amount. The units for the strike price is always ask asset / offer asset."
      ]
  , ""
  , "The date is the expiration for the contract. The contract expires at the end of the day (ie, midnight)."
  , ""
  , "You can specify multiple possible terms by seperating them with newlines:"
  , "10.000000, 2.1, 12/02/24"
  , "13.000000, 2.5, 12/21/24"
  , "15.000000, 2.7, 01/01/25"
  , ""
  , "The buyer of the proposal will choose one of the lines, and those terms will then be enforced."
  ]

proposalPaymentAddressMsg :: Text
proposalPaymentAddressMsg = unlines
  [ "What address should payments be made to?"
  , ""
  , unwords
      [ "When the proposal is purchased, the buyer will deposit the premium to this address."
      , "Then, when the contract is executed, the ask asset will also be deposit to this address."
      , "This address can be changed at any time during the life of the contract."
      ]
  ]

proposalFilterOfferAssetMsg :: Text
proposalFilterOfferAssetMsg = unlines
  [ "What asset will be given upon contract execution?"
  , ""
  , "The asset quantity must be in one of the following formats:"
  , "1. 'policy_id.asset_name'"
  , "2. 'ticker' - if in Ticker Registry"
  , "3. 'ADA' - if the asset is ada."
  , ""
  , "Fingerprints are not supported."
  ]

proposalFilterAskAssetMsg :: Text
proposalFilterAskAssetMsg = unlines
  [ "What asset will be deposited upon contract execution?"
  , ""
  , "The asset quantity must be in one of the following formats:"
  , "1. 'policy_id.asset_name'"
  , "2. 'ticker' - if in Ticker Registry"
  , "3. 'ADA' - if the asset is ada."
  , ""
  , "Fingerprints are not supported."
  ]

proposalFilterPremiumAssetMsg :: Text
proposalFilterPremiumAssetMsg = unlines
  [ "What asset will the premium be paid in?"
  , ""
  , "The asset quantity must be in one of the following formats:"
  , "1. 'policy_id.asset_name'"
  , "2. 'ticker' - if in Ticker Registry"
  , "3. 'ADA' - if the asset is ada."
  , ""
  , "Fingerprints are not supported."
  ]

proposalFilterSortMsg :: Text
proposalFilterSortMsg = unlines
  [ "Open options proposals can be sorted based off one of the following methods:"
  , "1. Lexicographically based on the UTxO's output reference"
  , "2. Chronologically based on the time the UTxO was created"
  , "3. Based on the amount of the offer asset"
  , "4. Based on the premiums"
  , "5. Based on the stike prices"
  , "6. Based on the expirations"
  , ""
  , "Option 3 is only available when the offer asset is set in the filter settings."
  , ""
  , unwords
      [ "Option 5 is only available when both the offer asset and the ask asset are set in"
      , "the filter settings."
      ]
  , ""
  , unwords
      [ "For options 4, 5, and 6, the smallest term will be used when the contracts are sorting in"
      , "ascending order. Meanwhile the largest term will be used when the contracts are sorted in"
      , "descending order."
      ]
  ]

premiumAssetMsg :: Text
premiumAssetMsg = unlines
  [ "Which asset do you want to use for the premium? Leave blank for any asset."
  , ""
  , "The asset name must be in one of the following formats:"
  , "1. 'policy_id.asset_name'"
  , "2. 'ticker' - if in Ticker Registry"
  , "3. 'ADA' - if the asset is ada."
  , ""
  , unwords
      [ "Fingerprints are not supported because there is no way for the p2p-wallet to know"
      , "which asset the fingerprint corresponds to unless it knows the asset in advance."
      , "Due to the composability of these DeFi protocols, users may not always have the offer asset"
      , "in question."
      ]
  ]

allProposalFilterSortMsg :: Text
allProposalFilterSortMsg = unlines
  [ "Options proposals can be sorted based off one of the following methods:"
  , "1. Based on the amount of the offer asset"
  , "2. Based on the premiums"
  , "3. Based on the stike prices"
  , "4. Based on the expirations"
  , ""
  , "Option 2 is only available when a premium asset is set while entering the trading pair to lookup."
  ]

optionsProposalsMsg :: Text
optionsProposalsMsg = unwords
  [ "Options proposals are options contracts that have not been purchased yet."
  , "These contracts cannot be executed until after they are purchased."
  ]

activeOptionsMsg :: Text
activeOptionsMsg = unwords
  [ "These are only the active options contracts that were sold by you. Options contracts you"
  , "purchased can be found though the Home page by looking for all Options Key NFTs you own."
  ]

