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

- When you first run the script it will detect that you have not yet generated the compiled **Fracada** plutus script. The script will then parameterize and compile it for you.
- It will then generate a new wallet for you. This wallet will be used to send transactions to the **Fracada** smart contract. The wallet must contain atleast `50 ADA` or `50,000,000 lovelace` so you will have to **top up** the wallet with some ADA to continue.

```bash
[2022/10/25 13:28:37] info: Welcome to Fracada Testnet Deployment Script
[2022/10/25 13:28:37] info: Payment Address: addr_test1vrlpfzjfu6000nrk8dvh0rvw2nxrhkdx7p7yrs5e9zcpe6c6u53dz
[2022/10/25 13:28:37] info: Tokens PolicyId: 7810cca2dc9a26c4865059925b66c34b39b4842616acda666e7be591
[2022/10/25 13:28:37] warn: Plutus Script not detected, Initializating...
[2022/10/25 13:28:39] info: Plutus Script Initialized!
[2022/10/25 13:28:39] info: Contract Address: addr_test1wr529sw72cjz4hk4g6q564ng8uzh4nut8lyyy0qyv3e6legc82wt6
[2022/10/25 13:28:39] info:
[Wallet Balance]
lovelace					8976009655
```

- If it has enough ADA it will bootstrap the wallet to prepare the test. Bootstrapping will do the following:
  - Mint two tokens `Token_A` and `Token_B`
  - It will also prepare the UTXOs in a way that is tuned for the test.

```bash
[2022/10/25 13:28:39] info: Bootstrapping wallet for Fracada end to end test...
[2022/10/25 13:28:39] info: Bootstrap txId: 0eb0c42882c7c40e5ba67cfe91c502f687ef0ba34dbb812b986976f641eae4be
[2022/10/25 13:28:39] info: Waiting for confirmation...
[2022/10/25 13:28:44] info:
[Wallet Balance]
lovelace					8971803806
Token_A_1666697317					1
Token_B_1666697317					1
```

- Once bootstraped it will now fractionalize `Token_A` to the fracada smart contract and you will get `100` unique fraction tokens in return, Ultimately locking the `Token_A` to the smart contract until the fraction tokens are burned.

```bash
[2022/10/25 13:28:44] info: Fractionalizing Token_A_1666697317...
[2022/10/25 13:28:48] info: Fractionlization txId: f0c60298faf165c839022a05d4e86b64c268288a80e493f109e7d03223829efb
[2022/10/25 13:28:48] info: Waiting for confirmation...
[2022/10/25 13:29:08] info: Fractionalization Complete!
[2022/10/25 13:29:08] info:
[Wallet Balance]
lovelace					8968058056
Token_B_1666697317					1
Fractions					100
```

- Optionally, **Fracada** allows adding you to add more `tokens` to be locked and be represented by the `100` unique fraction tokens that you just minted. Making it into an ***"Token Bag"***.

```
[2022/10/25 13:29:08] info: Adding Token_B_1666697317 to the contract...
[2022/10/25 13:29:09] info: AddNft txId: 77dc8e32f6509ee4186ff0759d99dc927db6e9858cbad550e67ea2fe4a469810
[2022/10/25 13:29:09] info: Waiting for confirmation...
[2022/10/25 13:29:21] info: Token_B_1666697317 added to the contract!
[2022/10/25 13:29:21] info:
[Wallet Balance]
lovelace					8967361146
Fractions					100
```

- Finally, you can unlock the tokens by burning the `100` fraction tokens.

```shell
[2022/10/25 13:29:21] info: Unlocking tokens...
[2022/10/25 13:29:23] info: Unlock txId: 39668e864579db00ea3b9569b2de674e19cbd081359ce912c1c520e331accb8b
[2022/10/25 13:29:23] info: Waiting for confirmation...
[2022/10/25 13:29:25] info: Unlocking Complete!
[2022/10/25 13:29:25] info:
[Wallet Balance]
lovelace					8969458292
Token_A_1666697317					1
Token_B_1666697317					1
```
