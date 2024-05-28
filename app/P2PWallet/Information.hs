module P2PWallet.Information where

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
