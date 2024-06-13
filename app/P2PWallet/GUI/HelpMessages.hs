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
  , mconcat $ intersperse " "
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
  , mconcat $ intersperse " "
      [ "The p2p-wallet will use this account index when pairing any hardware wallet keys with"
      , "this profile."
      ]
  , ""
  , mconcat $ intersperse " "
      [ "NOTE: Derivation paths can also be written with 'H' instead of '''; they are equivalent."
      , "For example, the path for the first payment key from account 0 can also be written as:"
      ]
  , "1852H/1815H/0H/0/0"
  , ""
  , mconcat $ intersperse " "
      [ "Since the single quote sometimes has a special meaning inside programming languages, the"
      , "'H' is occasionally used instead."
      ]
  ]

paymentAddressIndexMsg :: Text
paymentAddressIndexMsg = unlines
  [ "Hardware wallet key derivation paths take the form:"
  , "m / purpose' / coin_type' / account' / chain / address_index "
  , ""
  , mconcat $ intersperse " "
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
  , mconcat $ intersperse " "
      [ "You have full control over which payment key to use for each payment address."
      , "To keep things simple, it is recommended to go in order; jumping around with the"
      , "'address_index' will make it harder for you to remember which indices you are using."
      , "You will always be able to recover the address as long as you remember which 'account'"
      , "and 'address_index' were used in the payment key's deriviation path."
      ]
  , ""
  , mconcat $ intersperse " "
      [ "The p2p-wallet will use the account index set for this profile"
      , "when pairing the new hardware wallet keys."
      ]
  , ""
  , mconcat $ intersperse " "
      [ "NOTE: Derivation paths can also be written with 'H' instead of '''; they are equivalent."
      , "For example, the path for the first payment key from account 0 can also be written as:"
      ]
  , "1852H/1815H/0H/0/0"
  , ""
  , mconcat $ intersperse " "
      [ "Since the single quote sometimes has a special meaning inside programming languages, the"
      , "'H' is occasionally used instead."
      ]
  ]

stakeAddressIndexMsg :: Text
stakeAddressIndexMsg = unlines
  [ "Hardware wallet key derivation paths take the form:"
  , "m / purpose' / coin_type' / account' / chain / address_index "
  , ""
  , mconcat $ intersperse " "
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
  , mconcat $ intersperse " "
      [ "You have full control over which stake key to use for each stake address."
      , "To keep things simple, it is recommended to go in order; jumping around with the"
      , "'address_index' will make it harder for you to remember which indices you are using."
      , "You will always be able to recover the address as long as you remember which 'account'"
      , "and 'address_index' were used in the stake key's deriviation path."
      ]
  , ""
  , mconcat $ intersperse " "
      [ "The p2p-wallet will use the account index set for this profile"
      , "when pairing the new hardware wallet keys."
      ]
  , ""
  , mconcat $ intersperse " "
      [ "NOTE: Derivation paths can also be written with 'H' instead of '''; they are equivalent."
      , "For example, the path for the first payment key from account 0 can also be written as:"
      ]
  , "1852H/1815H/0H/0/0"
  , ""
  , mconcat $ intersperse " "
      [ "Since the single quote sometimes has a special meaning inside programming languages, the"
      , "'H' is occasionally used instead."
      ]
  ]

whatIsPairedWalletMsg :: Text
whatIsPairedWalletMsg = mconcat $ intersperse " "
  [ "A paired wallet is a hardware wallet address where the p2p-wallet knows about the associated"
  , "hardware wallet keys. Because it is aware of the keys, users can sign/witness any transaction"
  , "involving the paired wallet. The p2p-wallet only supports signing/witnessing transactions"
  , "for paired wallets!"
  ]

whatIsWatchedWalletMsg :: Text
whatIsWatchedWalletMsg = unlines
  [ mconcat $ intersperse " "
      [ "A watched wallet is an address where the p2p-wallet does NOT know about the associated"
      , "credentials. For example, if the user has a cold wallet where the keys are on an air-gapped"
      , "computer, there is no possible way for the p2p-wallet to safely automate the"
      , "signing/witnessing of transactions involving this address. However, the p2p-wallet can still"
      , "be used to build and submit transactions for these cold wallets. You can think of watched"
      , "wallets as windows into those addresses. Since the p2p-wallet can see the UTxOs located at"
      , "watched addresses, the transaction builder is able to use UTxOs from these addresses."
      ]
  , ""
  , mconcat $ intersperse " "
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
  [ mconcat $ intersperse " "
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
  , mconcat $ intersperse " "
      [ "The last option is only possible when search is being used for the UTxOs. This option"
      , "will be hidden if search is not being used. The asset that will be sorted on will"
      , "be the FIRST criteria used in the search. Make sure the first"
      , "criteria is actually an asset... To sort based off of a different native asset, move that"
      , "asset so that it is first in the search criteria."
      ]
  ]

depositSignMsg :: Text
depositSignMsg = unlines
  [ mconcat $ intersperse " "
      [ "Negative means the deposit was paid. Positive means the deposit was reclaimed."
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
  , "5. ticker"
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
  [ mconcat $ intersperse " "
      [ "Show only transactions after the specfied date. Leave it blank to show transactions since"
      , "the beginning. The default setting is to only show transactions within the past 30 days."
      ]
  ]

txEndDateMsg :: Text
txEndDateMsg = unlines
  [ mconcat $ intersperse " "
      [ "Show only transactions up through the specfied date. Leave it blank to show transactions"
      , "up through the present."
      ]
  ]

totalDelegatedMsg :: Text
totalDelegatedMsg = unlines
  [ mconcat $ intersperse " "
      [ "The total amount delegated across all payment addresses using this staking credential +"
      , "the current reward balance."
      ]
  ]

pledgeMsg :: Text
pledgeMsg = unlines
  [ mconcat $ intersperse " "
      [ "The amount of ADA the stake pool owners have committed as pledge. If this is not met,"
      , "the stake pool will be penalized."
      ]
  ]

activePledgeMsg :: Text
activePledgeMsg = unlines
  [ mconcat $ intersperse " "
      [ "The actual amount of ADA pledged by the stake pool owners. It must be at the pledge"
      , "amount to avoid penalties."
      ]
  ]

fixedCostMsg :: Text
fixedCostMsg = unlines
  [ mconcat $ intersperse " "
      [ "A fixed amount of ADA deducted from the total rewards to cover the stake pool's"
      , "operating expenses."
      ]
  ]

marginMsg :: Text
marginMsg = unlines
  [ mconcat $ intersperse " "
      [ "The percentage of the pool's total rewards that will be taken by the stake pool operator"
      , "after the fixed cost."
      ]
  ]

liveSaturationMsg :: Text
liveSaturationMsg = unlines
  [ mconcat $ intersperse " "
      [ "The percent saturation for this pool."
      ]
  ]

activeLinkedAddressesMsg :: Text
activeLinkedAddressesMsg = unlines
  [ mconcat $ intersperse " "
      [ "Payment addresses with non-zero balances that are using this staking credential."
      ]
  ]

spendableEpochMsg :: Text
spendableEpochMsg = unlines
  [ mconcat $ intersperse " "
      [ "The epoch when these rewards became spendable."
      ]
  ]

nativeAssetAreaEntryMsg :: Text
nativeAssetAreaEntryMsg = unlines
  [ "Native assets must use the full on-chain name such as:"
  , "10 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f7468657254"
  , ""
  , mconcat $ intersperse " "
      [ "You can go to another page and copy the asset name; then return here to paste it in."
      , "Your place will be saved."
      ]
  ]
