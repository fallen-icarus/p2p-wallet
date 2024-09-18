# P2P-DeFi Wallet

A fully p2p Cardano wallet with intrinsic support for a DEX, lending/borrowing, and options trading.
It includes a *transaction builder* that can be used to build arbitrarily complex DeFi compositions.
The transaction builder can even be used to manage multiple staking credentials in a single
transaction.

> [!WARNING]
> This is a prototype! Expect breaking changes until at least an official v1 release.

> [!NOTE]
> The [cardano-aftermarket protocol](https://github.com/fallen-icarus/cardano-aftermarket) (the
> secondary market for loans and options) has not been integrated yet. It is a WIP.

---
## Table of Contents
- [Abstract](#abstract)
- [Motivation](#motivation)
- [FAQ](#faq)
- [Screenshots](#screenshots)
- [Installing](#installing)

## Abstract

The P2P-DeFi wallet is the first of its kind wallet that enables users to build up arbitrarily
complex transactions. It functions like an online marketplace's shopping cart where uses add actions
to their cart until they are ready to "checkout". By enabling users to compose actions together into
a single transaction, users have access to significantly more economic opportunities (e.g.,
composing a DEX with an options contract execution) while saving 10x more money in the long run.

## Motivation

An overwhelming majority of Cardano's potential is wasted due to users being extremely limited in
what they can put in a transaction. If you have split your delegation across two staking
credentials, why does it require two transactions to update your delegation preferences (e.g., one
transaction per credential)? If you have a DEX position you intend to close, why can't you close the
position and send the money to a friend in the same transaction? Finally, if you want to buy an
options contract but don't have the required asset to buy it, why can't you exchange for the asset
on a DEX and use it to buy the options contract in the same transaction?

The reason why the answers to the above questions are universally "No" is a product of how DApps and
frontends (aka wallets) are designed. The eUTxO model natively supports *arbitrary* action
compositions within transactions. Developers are simply not taking advantage of it, and by
extension, they are preventing Cardano users from being able to take advantage of it.

There are two main reasons why developers need to start prioritizing support for composing actions
within Cardano transactions:

### Cost Savings

The transaction fee calculation for Cardano is:

```
fee = a * size_of_tx + b
```

where the current parameters are `a = 0.000044 ADA` and `b == 0.155381 ADA`
([source](https://cardano.stackexchange.com/questions/4472/calculation-of-transaction-fees)).
Substituting the parameters in to the equation results in:

```
fee = 0.000044 * size_of_tx + 0.155381
```

Every transaction you submit has a **minimum** fee of 0.15 ADA! Meanwhile, the cost per byte is dirt
cheap! Let's put it in real terms. Imagine you have the two staking credentials and want to update
both of their delegation preferences. A transaction with a single delegation certificate is
approximately 420 bytes in size which means the fee is approximately 0.173861 ADA. You need to pay
this twice (once per delegation transaction) so the total cost to you comes out to 0.347722 ADA.

But what if you combined the delegation updates into a single transaction? A transaction with two
delegation certificates is approximately 550 bytes in size which means the fee is approximately
0.179581 ADA. You pay literally have the transaction fees by combining the delegation updates into
one transaction! And it is just because you only have to pay the constant 0.155381 ADA once.

For every action that you are forced to use separate transactions for, you are forced to pay an
additional 0.155381 ADA in transaction fees. Take the delegation example to an extreme where you
want to update the delegation for 10 stake credentials. Using separate transactions would result in
1.7 ADA in fees. 

What about if they were all submitted in one transaction? Each delegation certificate is about 130
bytes so you would have a total transaction size of approximately 1590 bytes. This equates to a
transaction fee of 0.225341 ADA. That's almost 10x cheaper!

Perhaps you won't find yourself ever needing to update the delegation preferences for 10 staking
credentials, but what about updating the prices for 10 DEX positions? What about making loan
payments on 10 separate loans? When you consider DeFi, there are a lot of actions that *should* go
together in a single transaction. 

In order to maximize savings, every transaction should be as full as possible. If developers are not
allowing you to combine actions together into a single transaction, they are making you waste
money...

### Increased Economic Opportunities

All opportunities carry some form of risk. Whether you choose to go after the opportunity depends on
how comfortable you are with that risk. The less risky an opportunity, the more people will be
willing to go after the opportunity. For example, imagine if I offered you $1 million dollars, but
in order to claim it, you had to tightrope walk across two skyscrapers. Would you do it?

Most people would probably not. But what if I changed the terms to you had to drive from New York to
California within 1 week. The drive only takes 44 hours in total so even if you broke up the drive,
you would still make it on time! Would you take this deal? Pretty much anyone would take this deal.

The only difference between the two deals is the level of risk. While this example is extreme, it
highlights the fact that, in order to get more people involved in pursuing opportunities, minimizing
risk can do a lot.

When it comes to Cardano, is it better to have more people participating in DeFi or less? **This
isn't a zero-sum game.** If more people participate in Cardano DeFi, businesses will be incentivized
to also join in. This then feeds on itself since businesses provide services that attract more
users. On net, the following happens:

1. The value of everyone's ada increases relative to all other assets.
2. Stake pool rewards start to rise again as more transactions result in more transaction fees to
distribute among pool operators and delegators (see the [FAQ](#faq) for a continued discussion of
this).

The more people that participate in DeFi, the better off all ADA investors become. So, in addition
to creating cool new DeFi applications, we should strive to decrease the risk associated with DeFi.
One of the best ways to do this is to support composing actions in a single transaction.

Imagine if there was an options contract for sale in AGIX, but you have DJED to avoid the market
volatility of AGIX. You could convert DJED to AGIX in one transaction and then try to buy the
options contract in another transaction. But what if you don't get the options contract in the end
(e.g., if someone beat you to it)? You exposed yourself to the volatility of AGIX and now need to
convert it back to DJED in another transaction. Simply trying to go after this opportunity could
have cost you money.

But what if you could compose the conversion with the options purchase? If either the conversion
fails or the options purchase fail, the entire transaction would be invalidated. In other words, the
only scenario where you convert DJED to AGIX is the scenario where you immediately use it to buy the
options contract. You have **zero** market volatility risk, **zero** collateral risk, and you saved
money since you only paid the 0.155381 ADA once. Because this method is effectively risk-free,
significantly more people will try taking advantage of these DEX+Options opportunities. As a bonus,
the DEX and the options market even end up with greater liquidity!

Perhaps you find this example too contrived. If you do, then I encourage you to read how not being
able to compose DeFi actions in a single transaction can actually lead to [DeFi recessions and
market
instabilities](https://github.com/fallen-icarus/cardano-loans?tab=readme-ov-file#no-trustless-composability-with-other-dapps).

## The P2P-DeFi Wallet

The p2p-wallet is the first wallet frontend that actually enables users to take full advantage of
what Cardano has to offer:

- **Fully Permissionless DeFi** - no one can stop you from participating in DeFi. The p2p-wallet
uses the p2p-DeFi protocols which means you retain full custody, delegation control, and voting
control at all times. 
- **Arbitrarily Complex Transactions** - you can close a swap and use the proceeds to pay a friend
*in the same transaction*. You can create a swap, ask for a loan, execute an options contract, and
change your delegation preferences *in the same transaction*. If you can think of the composition,
the transaction builder can probably build it.
- **Notifications** - if any of your UTxOs change (personal or DeFi), the wallet will notify you the
next time you sync the wallets. Did someone execute your swap? Did the borrower accept your offer?
Was your options contract executed? The wallet will tell you.

## FAQ

### If composing actions in a single transaction decreases costs, how can it also increase the total transaction fees collected in an epoch?

While it is true that being able to avoid paying multiples of the 0.155381 ADA will reduce fees paid
by an individual Cardano users, its impact stops there. The increased economic opportunities, on the
other hand, has a network effect - more users/businesses attract more users/businesses. So while the
transaction fees collected by any individual user decreases, the exponential increase in total
number of users means the total transaction fees collected actually **grows**.

To put some numbers on it, if Alice decreases her total fees paid from 0.5 ADA to 0.17 ADA, while at
the same time 9 more users join and they each also pay 0.17 ADA, the total fees collected increased
from 0.5 ADA to 1.7 ADA. It tripled despite Alice personally cutting costs by 66%!

## Screenshots

## Installing

The p2p-wallet is effectively just a GUI wrapper around
[cardano-cli](https://github.com/IntersectMBO/cardano-cli) and
[cardano-hw-cli](https://github.com/vacuumlabs/cardano-hw-cli). The smart contracts for the p2p-DeFi
protocols depend on libsodium, scep256k1, and blst. Finally, the [GUI
library](https://github.com/fjvallarino/monomer) used to build the p2p-wallet depends on SDL2 and
GLEW.

> [!CAUTION]
> The GUI library is supposed to have cross-platform support for Windows, macOS and Linux. I have
> not been able to verify the wallet on anything but Linux.

> [!IMPORTANT] 
> This is not a software wallet! I do not know how to safely encrypt/decrypt hot keys, and so I can't
> add support for that. The wallet is fully capable of signing with hardware wallets directly, or
> exporting/importing the built transactions for signing manually.

> [!WARNING] 
> There is currently a bug in cardano-hw-cli that prevents hardware wallets from being able to sign
> DeFi transactions. You can find the official bug report
> [here](https://github.com/vacuumlabs/cardano-hw-interop-lib/issues/25) and the CIP discussion
> [here](https://github.com/cardano-foundation/CIPs/issues/871). Unfortunately, there is no timeline
> for a fix. Perhaps this wallet will create enough pressure to get it fixed ASAP.

### Prerequisites

#### cardano-cli and cardano-hw-cli

You will need both cardano-cli installed in order to build transactions and you will need
cardano-hw-cli installed if you wish to use a hardware wallet. You can find the latest cardano-cli
[here](https://github.com/IntersectMBO/cardano-node/releases) (it is included with the node
tarball). For cardano-hw-cli, you must use version 1.15.0-rc1, or later. You can find all of the
cardano-hw-cli releases [here](https://github.com/vacuumlabs/cardano-hw-cli/releases).

#### Ledger Wallets

If you are using a ledger hardware wallet on linux, you will need to add the udev rules so that you
don't need to use `sudo` every time to sign transactions. You can find the latest udev rules
[here](https://github.com/LedgerHQ/udev-rules). You can either manually add the `20-hw1.rules` file
to the `/etc/udev/rules.d/` directory, or you can execute the `add_udev_rules.sh` script using
`sudo` and it will do it for you.

#### Trezor Wallets

You will need to have [trezor-suite](https://trezor.io/trezor-suite) installed and open whenever you
wish to use your trezor hardware wallet. Alternatively, you can set up trezor-bridge by following
these [instructions](https://github.com/gitmachtl/scripts/tree/master/cardano/mainnet#how-to-prepare-your-system-before-using-a-hardware-wallet).

If you are on linux, you will also need udev rules for trezor so that you don't need to use `sudo`
every time you sign transactions. You can find those udev rules
[here](https://trezor.io/learn/a/udev-rules).

#### SDL2 and GLEW

The documentation for installing these dependencies can be found
[here](https://github.com/fjvallarino/monomer/blob/main/docs/tutorials/00-setup.md#libraries-sdl2-and-glew).

### Building From Source

#### Install the necessary packages - similar to cardano-node
```bash
sudo apt update
sudo apt upgrade
sudo apt-get install autoconf automake build-essential curl g++ git jq libffi-dev libgmp-dev libncursesw5 libssl-dev libsystemd-dev libtinfo-dev libtool make pkg-config wget zlib1g-dev liblzma-dev libpq-dev
```

#### Install GHC and cabal
```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

- Prepend or append the required PATH variable.
- You do not need to install the haskell-langauge-server.
- You do not need to install stack.
- Install the required packages. You can keep this terminal window open and install from another
window.
- Press ENTER to proceed.

```bash
source $HOME/.bashrc
ghcup install ghc 9.2.8 # The wallet relies on 9.2.8
```

#### Install libsodium, scep256k1, and blst
```bash
git clone https://github.com/intersectmbo/libsodium
cd libsodium
git checkout dbb48cc
./autogen.sh
./configure
make
sudo make install

cd ../ # Leave the libsodium directory.
git clone https://github.com/bitcoin-core/secp256k1
cd secp256k1
git checkout ac83be33
./autogen.sh
./configure --enable-module-schnorrsig --enable-experimental
make
make check
sudo make install
sudo ldconfig

cd ../ # Leave the secp256k1 directory.
git clone https://github.com/supranational/blst
cd blst
git checkout v0.3.10
./build.sh
cat > libblst.pc << EOF # This command extends until the next EOF
prefix=/usr/local
exec_prefix=\${prefix}
libdir=\${exec_prefix}/lib
includedir=\${prefix}/include

Name: libblst
Description: Multilingual BLS12-381 signature library
URL: https://github.com/supranational/blst
Version: 0.3.10
Cflags: -I\${includedir}
Libs: -L\${libdir} -lblst
EOF
sudo cp libblst.pc /usr/local/lib/pkgconfig/
sudo cp bindings/blst_aux.h bindings/blst.h bindings/blst.hpp  /usr/local/include/
sudo cp libblst.a /usr/local/lib
sudo chmod u=rw,go=r /usr/local/{lib/{libblst.a,pkgconfig/libblst.pc},include/{blst.{h,hpp},blst_aux.h}}
```

### Build the executable - this may take about 30 minutes
```bash
cd ../ # Leave the blst directory.
git clone https://github.com/fallen-icarus/p2p-wallet
cd p2p-wallet
git checkout dev # this branch
cabal clean
cabal update
cabal build
```

The `p2p-wallet` executable program should now be at
`dist-newstyle/build/x86_64-linux/ghc-9.2.8/p2p-wallet-0.1.0.0/x/p2p-wallet/build/p2p-wallet/p2p-wallet`.
Move the program to somewhere in your `$PATH`.

You can open the wallet by executing `p2p-wallet` in the terminal.

> [!NOTE] 
> I currently do not know how to package all of these dependencies up to create a single desktop
> application.
