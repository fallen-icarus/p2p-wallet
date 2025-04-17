# Revision history for p2p-wallet

## 0.2.6

### Fixes
* Editing a limit order creation to sell would mess up the units ([#14](https://github.com/fallen-icarus/p2p-wallet/issues/14)).

## 0.2.5

This version is the same as 0.2.4 except it also contains documentation for the new AppImage. A new
tag was needed so that the Github Release would link to the right commit.

## 0.2.4

### Features
* Added total cpu and memory usage to builder logs.
* Added ability for users to specify custom deposit amounts for Claim Bids. Since the bidder can
technically walk away even after the seller accepts the bid, the deposit is used to compensate the
seller if the bidder does walk away. If the deposit is too small, the seller may prefer not to
accept Claim Bids since it means they will miss out on other, more committed, bidders. A large
deposit can make sellers take the Claim Bids more seriously. 

### Fixes
* Fixed aftermarket notification bug for expired claim bids.
* Fixed not being able to scroll when sale and bid information doesn't fit on low resolution
monitors.
* Fixed building transactions with aftermarket spot purchases and accepted bid claims from different
markets.
* Fixed formatting of bid creation menu when only a Claim Bid is possible.
* Sale updates and bid updates were not counted as tx inputs by the builder.
* ADA in the bid wasn't counting towards minUTxOValue when claiming NFTs from an accepted bid.

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
