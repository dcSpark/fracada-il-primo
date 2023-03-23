
# Fracada il primo operations guide

This dApp expands on fracada, enabling adding more NFTs to the contract and mint more tokens after the intial minting.
Here we describe the steps to execute for each functionality.

## Configuration

The different scripts will read the configuration
The following environment variables must be defined:

* NFT_CURRENCY
    Currency id of the first NFT to lock in fracada.
* NFT_TOKEN
    Token name of the first NFT to lock in fracada.
* FRACT_TOKEN
    Token name of the fractional tokens (currency id for fractional tokens is generated)
* MIN_SIGS
    Minimun number of signatures required to approve adding more NFTs or minting more fractional tokens

Example (`demo/demo_params.sh`)

```sh
export NFT_CURRENCY="58e4abca68b2f61f8ec0a930016ab0817dfdc0d5183587f10fa501fb"
export NFT_TOKEN="FakeNFT_A"
export FRACT_TOKEN="FracadaTokenA"
export MIN_SIGS=2
```

### `params.sh`

Initial nft asset and factional asset derived from the environment variables defined above.

### `config.sh`

node and network parameters required by cardano-cli.

### `sign_send.sh`

This script signs and submits the transaction to the chain. It takes a skey name and assumes its stored in the address folder (see demo/address)
Feel free to modify it to suit your security needs.

## Initial setup

Before executing the lock and mint script, we need to generate validator and minting (.plutus) contracts, the contract address, and fractional asset currency id.
The initial NFT to lock, the fraction token name, and the minimum number of signatures required will be read from the environment (see `demo/demo_params.sh`).
Requires a list of extended public keys paths given in the `authorized_keys.txt` file. (see `demo/authorized_keys.txt`)
The command is invoked with:

```sh
init_validator.sh
```

It will generate the following output files:

* `validator.plutus` - onchain code of the validator script
* `validator-hash.txt` - validator hash
* `wallets/validator.addr` - onchain address of the dApp
* `minting.plutus` - onchain code of the minting script
* `currency-id.txt` - currency id of fractional tokens

## Lock NFT and initial mint

To lock the initial NFT and mint the fractional tokens, run:

```sh
lock_nft.sh {wallet} {initial amount to mint}
```

Where `{wallet}` is the name of the `.addr` file in wallet folder that contains the address of the wallet holding the NFT, and `{initial amount to mint}` is the number of fractional tokens to mint. You will be prompted to select the NFT and collateral utxo, and the transaction will be built, signed, and sent (using `sign_send.sh`).
The current datum will be saved in the `datum.json` file.

## Include additional NFTs in the contract

To add a new NFT to the contract, it first must be approved by the authorized signatures (defined in the initial setup in the `authorized_keys.txt` file ), so the process is broken into three steps:

* generate the datum and datum hash based on the new NFT to add
* Enough approvers signed the datum-hash.txt
* build the transaction adding the signed hashes to the redeemer

### Generate datum and hash to be signed for adding a new NFT

Calling

```sh
add_nft.sh {nft currency id} {nft token name}
```

Will generate a shell script to be called when the signatures have been collected, in the form of `continue_add_nft_{nft currency id}_{nft token name}.sh`

### Signing the hash to add a new NFT

Approvers can sign the datum hash with:

```sh
bin/sign datum-hash.txt {signing key file} {output file}
```

(see `demo/sign_with_all.sh` for an example)

### Build and submit the transaction to add a new NFT

When you collect a sufficient number of signatures, the process can continue and the transaction can be submitted. Calling:  

```sh
continue_add_nft_{nft currency id}_{nft token name}.sh {wallet}
```

Will prompt you to select the UTxO containing the NFT to be added from {wallet}, the UTxO that locks all the NFTs already in the contract, and the UTxO for collateral.
It will build the redeemer with the sign files listed in `signaturefiles.txt` and submit the transaction to the chain.

## Mint more fractional tokens

To mint more fractional tokens, it first must be approved by the authorized signatures (defined in the initial setup in the `authorized_keys.txt` file ), so the process is broken into three steps:

* generate the datum and datum hash updating the total count of fractional tokens minted
* Enough approvers signed the datum-hash.txt
* build the transaction adding the signed hashes to the redeemer

### Generate datum and hash to be signed to mint more

Calling

```sh
mint_more.sh {extra amount to mint}
```

Will read the current datum and generate a new `datum.json` file updated with the total amount of fractional tokens minted and the corresponding `datum-hash.txt` file to be distributed for signing.
The script will also generate a shell script to be called when the signatures have been collected, in the form of `continue_mint_${extra amount to mint}.sh`

### Signing the mint more hash

Approvers can sign the datum hash with:

```sh
bin/sign datum-hash.txt {signing key file} {output file}
```

(see `demo/sign_with_all.sh` for an example)

### Build and submit the mint more transaction

When you collect a sufficient number of signatures, the process can continue and the transaction can be submitted. Calling:  

```sh
continue_mint_${extra amount to mint}.sh {wallet}
```

Will prompt you to select the UTxO that locks all the NFTs already in the contract, and the UTxO for collateral.
It will build the redeemer with the sign files listed in `signaturefiles.txt` and submit the transaction to the chain.

## Return all NFTs

To unlock all the NFTs from the contract, the wallet has to burn all the fractional tokens.
Calling:

```sh
return_nft.sh {wallet} {total tokens to burn}
```

Will prompt you to select the UTxO containing the fractional tokens to burn from {wallet}, the UTxO that contains all the NFTs in the contract, and the UTxO for collateral. It will sign and send the transaction, burning the fractional tokens and paying the NFTs to the wallet.
