<h1 align="center">
  Fracada Il Primo
</h1>
<p align="center">Testnet Demo Script</p>

## Requirements

This script tries to simplify testing of the Fracada smart contract. But you will still need to have a few things setup in your system to be able to test it. The following are the requirements:

- `nix-shell` to be able to build the **Fracada** plutus code.
- `cardano-node` and `cardano-cli` added to your `$PATH` environment variable. Your node must be fully synced to the **Cardano Testnet** before you can run this script.
- `nodejs` version `v14.17.4` or just do `nvm use`.

## Getting started

Simply run `npm install` and `npm start` after entering the directory `fracada-il-primo/testnet_demo`.

## How it works

- When you first run the script it will detect that you have not yet generated the compiled **Fracada** plutus script. The script will then parameterize it for you and compile it.
- It will then generate a new wallet for you. This wallet will be used to send transactions to the **Fracada** smart contract. The wallet must contain atleast `50 ADA` or `50,000,000 lovelace` so you will have to **top up** the wallet with some ADA to continue.

```bash
[2022/04/14 11:18:18] info: Welcome to Fracada Testnet Deployment Script
[2022/04/14 11:18:18] info: Payment Address: addr_test1vz6jdcfhnfujkwf50hsgzgqguk8p0vgfmgttcxkg9sl203gty6fw0
[2022/04/14 11:18:18] info: Fake NFT PolicyId: f6bad380158199e1d6d98bf7b6deb642249157521da7333e9353b7ec
[2022/04/14 11:18:18] info: Contract Address: addr_test1wznxekyzaxfgdhrw0glsqavked23ue0uteyxc5s4cvd4mksvk5cg3
[2022/04/14 11:18:18] info: 
[Wallet Balance] 
lovelace                                        57139260
```

- If it has enough ADA it will bootstrap the wallet to prepare the test. Bootstrapping will do the following:
  - Mint two fake NFTs `FakeNFt_A` and `FakeNft_B`
  - It will also prepare the UTXOs in a way that is tuned for the test.

```bash
[2022/04/14 11:18:18] info: Bootrapping wallet for Fracada end to end test...
[2022/04/14 11:18:19] info: Bootstrap txId: 7058ac480ff054c25dfb1b104df95de0601295c31c944556f9345d904147e3d3
[2022/04/14 11:18:19] info: Waiting for confirmation...
[2022/04/14 11:18:31] info: 
[Wallet Balance] 
lovelace                                        52938911
FakeNft_A_1649692137                                    1
FakeNft_B_1649692137                                    1
```

- Once bootstraped it will now fractionalize `FakeNft_A` to the fracada smart contract and you will get `100 NftFractions` in return, Ultimately locking the `FakeNft_A` to the smart contract until the `NftFractions` tokens are burned.

```bash
[2022/04/14 11:18:31] info: Fractionalizing FakeNft_A...
[2022/04/14 11:18:33] info: Fractionlization txId: b6e8aaa0bf680c761857a6db35fb1f9c14dab54bc99341a653c9625793a039b7
[2022/04/14 11:18:33] info: Waiting for confirmation...
[2022/04/14 11:18:45] info: Fractionalization Complete!
[2022/04/14 11:18:45] info: 
[Wallet Balance] 
lovelace                                        50420449
FakeNft_B_1649692137                                    1
NftFractions                                    100
```

- Optionally, **Fracada** allows adding you to add more `tokens` to be locked and be presented by the `100 NftFractions` that you just minted. Making it into an ***"Token Bag"***. 

```
[2022/04/14 11:18:45] info: Adding FakeNft_B to the contract...
[2022/04/14 11:18:52] info: AddNft txId: cc04c42d9b01275d7427df24cd47760397dbbb1c44e58e5ee7f33307545533f7
[2022/04/14 11:18:52] info: Waiting for confirmation...
[2022/04/14 11:19:25] info: FakeNft_B added to the contract!
[2022/04/14 11:19:26] info: 
[Wallet Balance] 
lovelace                                        49231627
NftFractions                                    100
```

- Finally, you can unlock the tokens by burning the `100 NftFractions`.

```shell
[2022/04/14 11:19:26] info: Unlocking FakeNFTs...
[2022/04/14 11:19:27] info: Unlock txId: c60b647c34c5d1356d4575b141e3ffee35f40b88cf179a57516cfa2778d1c30f
[2022/04/14 11:19:27] info: Waiting for confirmation...
[2022/04/14 11:19:39] info: Unlocking Complete!
[2022/04/14 11:19:39] info: 
[Wallet Balance] 
lovelace                                        50380210
FakeNft_A_1649692137                                    1
FakeNft_B_1649692137                                    1
```
