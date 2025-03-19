#!/bin/bash

## The latest p2p-wallet version number.
wallet_version=0.2.5

## Prepare the AppDir. Remove the old one if it still exists.
rm -rf AppDir
mkdir -p AppDir/usr/bin AppDir/usr/lib

## Copy the cardano icon to the AppDir.
cp assets/icons/cardano.svg AppDir/cardano_icon.svg

## Create the AppRun executable. This expression needs to be saved to the file EXACTLY.
## It should not be expanded.
echo '#!/bin/bash
HERE="$(dirname "$(readlink -f "${0}")")"
export PATH="${HERE}/usr/bin/:${PATH}"
export LD_LIBRARY_PATH="${HERE}/usr/lib/:${LD_LIBRARY_PATH}"
export PKG_CONFIG_PATH="${HERE}/usr/lib/pkgconfig:${PKG_CONFIG_PATH}"
exec "${HERE}/usr/bin/p2p-wallet" "$@"' > AppDir/AppRun

## Make it executable.
chmod +x AppDir/AppRun

## Create the .desktop file.
cat > AppDir/p2p-wallet.desktop << EOF
[Desktop Entry]
X-AppImage-Arch=x86_64
X-AppImage-Version=${wallet_version}
X-AppImage-Name=p2p-wallet
Name=p2p-wallet
Exec=usr/bin/p2p-wallet
Icon=cardano_icon
Type=Application
Terminal=false
Categories=Utility;
Comment=
EOF

## Build the `p2p-wallet` executable.
cabal update
cabal build exe:p2p-wallet
cp "$(cabal list-bin exe:p2p-wallet)" AppDir/usr/bin/

## Download and configure `cardano-cli`.
cardano_cli_version=10.3.0.0
curl -L "https://github.com/IntersectMBO/cardano-cli/releases/download/cardano-cli-${cardano_cli_version}/cardano-cli-${cardano_cli_version}-x86_64-linux.tar.gz" | tar zx -C AppDir/usr/bin/
mv AppDir/usr/bin/cardano-cli* AppDir/usr/bin/cardano-cli # Drop version suffix from executable.
chmod +x AppDir/usr/bin/cardano-cli

## Download and configure `cardano-hw-cli`.
cardano_hw_cli_version=1.17.0
curl -L "https://github.com/vacuumlabs/cardano-hw-cli/releases/download/v${cardano_hw_cli_version}/cardano-hw-cli-${cardano_hw_cli_version}_linux-x64.tar.gz" | tar zx -C AppDir/usr/bin/
mv AppDir/usr/bin/cardano-hw-cli/ AppDir/usr/bin/cardano-hw-cli-dir/ # Rename sub-directory since it shares name with executable.
mv AppDir/usr/bin/cardano-hw-cli-dir/* AppDir/usr/bin/ # Move all files out of sub-directory.
rm -r AppDir/usr/bin/cardano-hw-cli-dir/ # Delete sub-directory.
chmod +x AppDir/usr/bin/cardano-hw-cli

## Instead of installing the dependencies, only the required files needed for static/dynamic
## linking are included. `ldd $(cabal list-bin exe:p2p-wallet)` was used to determine which
## files were required for linking. Dependencies that are usually included by modern systems
## are excluded.
cp /lib/x86_64-linux-gnu/libFLAC.so.8 AppDir/usr/lib/
cp /lib/x86_64-linux-gnu/libGLEW.so.2.2 AppDir/usr/lib/
cp /lib/x86_64-linux-gnu/libGLU.so.1 AppDir/usr/lib/
cp /lib/x86_64-linux-gnu/libSDL2-2.0.so.0 AppDir/usr/lib/
cp /lib/x86_64-linux-gnu/libXcursor.so.1 AppDir/usr/lib/
cp /lib/x86_64-linux-gnu/libXss.so.1 AppDir/usr/lib/ # Missing on Fedora.
cp /lib/x86_64-linux-gnu/libasyncns.so.0 AppDir/usr/lib/
cp /lib/x86_64-linux-gnu/libdecor-0.so.0 AppDir/usr/lib/
cp /lib/x86_64-linux-gnu/libogg.so.0 AppDir/usr/lib/
cp /lib/x86_64-linux-gnu/libopus.so.0 AppDir/usr/lib/
cp /lib/x86_64-linux-gnu/libsndfile.so.1 AppDir/usr/lib/
cp /lib/x86_64-linux-gnu/libtinfo.so.6 AppDir/usr/lib/
cp /lib/x86_64-linux-gnu/libvorbis.so.0 AppDir/usr/lib/
cp /lib/x86_64-linux-gnu/libvorbisenc.so.2 AppDir/usr/lib/
cp /usr/lib/x86_64-linux-gnu/pulseaudio/libpulsecommon-15.99.so AppDir/usr/lib/
cp /usr/local/lib/libsecp256k1.so.0 AppDir/usr/lib/
cp /usr/local/lib/libsodium.so.23 AppDir/usr/lib/

## Create the AppImage using `appimagetool`.
## Source: https://github.com/AppImage/appimagetool
appimagetool AppDir/
