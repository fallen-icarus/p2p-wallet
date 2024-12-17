# Revision history for p2p-wallet

## Next

### Features
* Set ADA as default collateral for all new loans. This way the minUTxOValue will count towards the
collateral.

### Fix
* "Open Asks" page label missing trailing 's'.

### Dependencies
* Switch from `cryptonite` to `crypton`. The former is deprecated.

## 0.2.1

### Fixes
* Made DEX "Trade" page scrollable in case the order book doesn't fit on the screen for lower
resolutions.

## 0.2.0

### Features
* When delegating to a DRep, set the default option to always-abstain ([#6](https://github.com/fallen-icarus/p2p-wallet/issues/6)).

### Fixes
* Fix pool picker getting stuck in a loop ([#4](https://github.com/fallen-icarus/p2p-wallet/issues/4)).
See [commit message](https://github.com/fallen-icarus/p2p-wallet/commit/5128e28ff2d29528193ef0de91874940c078a302) for more details.

## 0.1.0 - Demo Version

* Prototype released on an unsuspecting world.
