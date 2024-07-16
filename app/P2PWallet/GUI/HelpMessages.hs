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

txSearchMsg :: Text
txSearchMsg = unlines
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
  , ""
  , unwords
      [ "Fingerprints are not supported because there is no way for the p2p-wallet to know"
      , "which asset the fingerprint corresponds to unless it knows the asset in advance."
      , "Due to the composability of cardano-swaps, users may not always have the offer asset"
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

offerAssetFilterMsg :: Text
offerAssetFilterMsg = unlines
  [ "Filter open positions for those selling a specific asset."
  , ""
  , "The asset name must be in one of the following formats:"
  , "1. 'policy_id.asset_name'"
  , "2. 'ticker' - if in Ticker Registry"
  , ""
  , "Fingerprints are not supported."
  ]

askAssetFilterMsg :: Text
askAssetFilterMsg = unlines
  [ "Filter open positions for those buying a specific asset."
  , ""
  , "The asset name must be in one of the following formats:"
  , "1. 'policy_id.asset_name'"
  , "2. 'ticker' - if in Ticker Registry"
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
