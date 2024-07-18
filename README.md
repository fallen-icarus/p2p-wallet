## Installing

### Prerequisites

#### cardano-cli and cardano-hw-cli

You will need both cardano-cli installed in order to build transactions and you will need
cardano-hw-cli installed if you wish to use a hardware wallet. You can find the latest cardano-cli
[here](https://github.com/IntersectMBO/cardano-node/releases) (it is included with the node
tarball). For cardano-hw-cli, you must use version 1.15.0-rc1, or later. You can find all of the
cardano-hw-cli releases [here](https://github.com/vacuumlabs/cardano-hw-cli/releases).

#### Ledger Wallets

If you are using a ledger hardware wallet on linux, you will need to add the udev rules so that you
don't need to use `sudo` every time. You can find the latest udev rules
[here](https://github.com/LedgerHQ/udev-rules). You can either manually add the `20-hw1.rules` file
to the `/etc/udev/rules.d/` directory, or you can execute the `add_udev_rules.sh` script using
`sudo` and it will do it for you.

#### Trezor Wallets

You will need to have [trezor-suite](https://trezor.io/trezor-suite) installed and open whenever you
wish to use your trezor hardware wallet. Alternatively, you can set up trezor-bridge by following
these [instructions](https://github.com/gitmachtl/scripts/tree/master/cardano/mainnet#how-to-prepare-your-system-before-using-a-hardware-wallet).

If you are on linux, you will also need udev rules for trezor so that you don't need to use `sudo`
every time. You can find those udev rules [here](https://trezor.io/learn/a/udev-rules).

#### SDL2 and GLEW

The documentation for installing these dependencies can be found
[here](https://github.com/fjvallarino/monomer/blob/main/docs/tutorials/00-setup.md#libraries-sdl2-and-glew).

### The Desktop Wallet

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
cabal clean
cabal update
cabal build
```

The `p2p-wallet` executable program should now be at
`dist-newstyle/build/x86_64-linux/ghc-9.2.8/p2p-wallet-0.1.0.0/x/p2p-wallet/build/p2p-wallet/p2p-wallet`.
Move the program to somewhere in your `$PATH`.

You can open the wallet by executing `p2p-wallet` in the terminal.

TODO: Add a desktop shortcut for opening the wallet.

## Notes on Derivation Paths

This may be needed to explain different keys since some wallets do not use the hardened paths:

https://learnmeabitcoin.com/technical/keys/hd-wallets/derivation-paths/#notation
