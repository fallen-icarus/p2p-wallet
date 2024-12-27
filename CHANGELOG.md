# Revision history for p2p-wallet

## 0.2.3

### Features
* Add red text box border for required fields on the Options page.

### Fixes
* Fixed formatting when direct payments are to an un-tracked address.

## 0.2.2

### Features
* Set ADA as default collateral for all new loans. This way the minUTxOValue will count towards the
collateral.

### Fixes
* "Open Asks" page label missing trailing 's'.
* "Open Offers" sort help message wasn't showing the correct message.
* Tx Builder would drop the staking script observer for interest/penalty applications if loan
payments were made in the same transaction.
* Fix formatting when viewing information for a specific loan key from the Home page.

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
