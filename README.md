<h1 align="center">
  Fracada Il Primo
</h1>
<p align="center">Advanced Plutus fractionalized NFT protocol which supports "NFT Bag" functionality.</p>

<p align="center"><img src="https://img.shields.io/badge/license-mit-blue?style=for-the-badge&logo=none" alt="license" /></p>

## Disclaimer

The code on this repository has **not** been audited. We don't recommend it using in production without a full security audit. Use it at your own risk!.

## Protocol

This contract locks an NFT and mints a number of tokens representing fractions of it. To get the NFT back, the fraction tokens are burned.

The protocol has three steps:

1. Locking the NFT: The NFT is paid to the contract
2. Mint tokens: Fraction tokens are minted (must be run by the same person who performed step 1).
3. Return the NFT: Burning all the fraction tokens will allow the user to redeem the NFT back.

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
$ cabal test
cabal test
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
    Can lock NFT, mint fractional tokens, and exchange the NFT back when burning the tokens:                      OK (0.22s)
    Full scenario (lock NFT with minting, add more NFTs, mint more tokens, return all NFTs in exchange of tokens: OK (0.43s)
    No new NFTs (lock NFT with minting, mint more tokens, return all NFTs in exchange of tokens:                  OK (0.28s)
    Full scenario (lock NFT with minting, mint more tokens, add more NFTs, return all NFTs in exchange of tokens: OK (0.44s)
    Can't return the nft if fractional tokens aren't burned:                                                      OK (0.13s)
    Can't return the nft if not all fractional tokens aren't burned:                                              OK (0.13s)
    Can't mint fractional tokens:                                                                                 OK (0.12s)
    Can't add unsigned nft:                                                                                       OK (0.12s)
    Can't mint unsigned tokens:                                                                                   OK (0.15s)
    Can't add more than one token:                                                                                OK (0.14s)
    Can't steal NFTs when minting:                                                                                OK (0.20s)
    Can't mint extraneous tokens:                                                                                 OK (0.15s)
    Can't add the same NFT:                                                                                       OK (0.17s)
    datum not updated adding NFTs:                                                                                OK (0.11s)
    Must return all NFTs:                                                                                         OK (0.21s)
    Can't mint different than declared:                                                                           OK (0.05s)
    Can't mint if not locked:                                                                                     OK (0.05s)
    Can't burn if NFTs not returned:                                                                              OK (0.14s)

All 19 tests passed (3.27s)
Test suite fracada-test: PASS
1 of 1 test suites (1 of 1 test cases) passed.
```

## User Scripts
