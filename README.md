<h1 align="center">
  Fracada Il Primo
</h1>
<p align="center">Advanced Plutus fractionalized Token protocol which supports "Token Bag" functionality.</p>

<p align="center"><img src="https://img.shields.io/badge/license-mit-blue?style=for-the-badge&logo=none" alt="license" /></p>

## Disclaimer

The code on this repository has **not** been audited. We don't recommend it using in production without a full security audit. Use it at your own risk!.
If any of the fraction tokens are lost, locked Token(s) are locked forever.

## User guarantee

User of the "Token Bag" and buyer of fraction tokens must verify that the right validity token is present in the "Token Bag".
Right validity token is the one with CurrencySymbol of the minting policy provided with this smart contract.

## Parties

Person creating the "Token Bag"
 - Locks initial Token
 - Mints initial batch of fraction tokens
 - Decides on who the signatories are
 - Decides minumum number of signatories needed for optional actions

Signatories approve of optional actions

## Protocol

Script is only valid if validity token is present in the UTxO. It has a fixed TokenName and shares a CurrencySymbol with fraction tokens.

This contract locks a Token in "Token Bag" and mints a number of tokens representing fractions of the "Token Bag".
Optionally, it can lock additional Tokens in the "Token Bag"
Optionally, it can mint a number of additional tokens representing fractions of the "Token Bag"
To spend the "Token Bag", **ALL** of the fraction tokens are burned.

**IMPORTANT** if any of the fraction tokens are lost in the meantime, locked Token(s) are locked forever.

The protocol has three steps, and two optional:

1. Locking the Token: The Token is paid to the contract and "Token Bag" is created
2. Mint tokens: Fraction tokens are minted (must be run by the same person who performed step 1). Fraction tokens represent fractions of the "Token Bag".
3. (Optional) Locking additional Tokens: More Tokens are locked in the "Token Bag", which signatories must approve.
4. (optional) Minting additional tokens: More fraction tokens are minted, which signatories must approve. Fraction tokens represent fractions of the "Token Bag".
5. Return the Token(s): Burning all the fraction tokens will allow unlocking all of the Tokens from the "Token Bag".

## Validator

Validates :
- minting more fractions of the "Token Bag"
- adding Tokens to the "Token Bag"
- spending the "Token Bag"

Datum stores :
- AssetClass of fraction tokens
- total number of fraction tokens minted
- list of signatories
- minimum number of signers needed for optional actions

Initial Datum is guaranteed by validity token in minting policy.
After that the validator makes sure that it stays in the script by permitting only one input and only one output UTxO

Has no Redeemer.

## Minting policy

Mints
- fraction tokens
- validity token

Validity token has fixed TokenName. It is used to provide guarantee for the Datum.
It shares CurrencySymbol with fraction tokens.

Fraction tokens' TokenName is calculated from UTxO spent in creating the "Token Bag" and is unique for each "Token Bag".

UTxO to be spent is passed as a Redeemer.

## Building

To build the project execute `cabal build` at the project root.

To build:

``` bash
$ nix-shell
...
$ cabal build
...
```

## Testing

To run use-case test execute the following commands at the project root.

``` bash
$ nix-shell
$ cabal test --test-show-details=streaming
Build profile: -w ghc-8.10.4.20210212 -O1
In order, the following will be built (use -v for more details):
 - fracada-il-primo-0.1.0.0 (test:fracada-test) (ephemeral targets)
Preprocessing test suite 'fracada-test' for fracada-il-primo-0.1.0.0..
Building test suite 'fracada-test' for fracada-il-primo-0.1.0.0..
Running 1 test suites...
Test suite fracada-test: RUNNING...
use cases
  fracada
    Expose endpoints:                                                                                             OK (0.01s)
    Can lock NFT, mint fractional tokens, and exchange the NFT back when burning the tokens:                      OK (0.17s)
    Full scenario (lock NFT with minting, add more NFTs, mint more tokens, return all NFTs in exchange of tokens: OK (0.28s)
    No new NFTs (lock NFT with minting, mint more tokens, return all NFTs in exchange of tokens:                  OK (0.22s)
    Full scenario (lock NFT with minting, mint more tokens, add more NFTs, return all NFTs in exchange of tokens: OK (0.31s)
    AuthorizedPubKeys must be a list of unique values:                                                            OK (0.05s)
    Can't return the nft if fractional tokens aren't burned:                                                      OK (0.08s)
    Can't return the nft if not all fractional tokens aren't burned:                                              OK (0.12s)
    Can't mint fractional tokens:                                                                                 OK (0.10s)
    Can't add unsigned nft:                                                                                       OK (0.08s)
    Can't mint unsigned tokens:                                                                                   OK (0.09s)
    Can't add more than one token:                                                                                OK (0.07s)
    Can't steal NFTs when minting:                                                                                OK (0.14s)
    Can't mint extraneous tokens:                                                                                 OK (0.13s)
    Can't add the same NFT:                                                                                       OK (0.11s)
    Can't change datum when adding NFTs:                                                                          OK (0.08s)
    Must return all NFTs:                                                                                         OK (0.15s)
    Must record correct fraction tokens asset class:                                                              OK (0.06s)
    Can't add Fraction Token to the bag:                                                                          OK (0.09s)
    Can't add more NFTs than limit:                                                                               OK (0.62s)
    Can't mint different than declared:                                                                           OK (0.02s)
    Can't mint different than declared when inputting arbitrary UTxO with a datum:                                OK (0.05s)
    Can't mint without creating contract UTxO:                                                                    OK
    Can't burn if NFTs not returned:                                                                              OK (0.06s)

All 24 tests passed (3.24s)
Test suite fracada-test: PASS
1 of 1 test suites (1 of 1 test cases) passed.
```

### Trace
There is also an executable trace at `test/Trace.hs` to manually test a scenario if detailed debugging info is needed.
To run execute the command `cabal run trace`.

### Testing UTXO limit
There is an executable for testing the limit of NFTs which can be added to the bag. Test is in the file `test/TestUtxoLimit.hs`
and there is a constant `addTokenRepetitions` which means how many time a new token is added. After each change in the validator,
this script must be updated accordingly and run to determine the new limit.

Testing must be done manually by increasing/decreasing the constant and the goal is to find the maximum number when test is passing.

To run the test execute: `cabal run test-utxo-limit`.

## User Scripts
