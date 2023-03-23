
# Fracada Il Primo operations guide

This dApp expands on Fracada, enabling adding more tokens to the contract and mint more tokens after the intial minting.  
Here we describe the steps to execute for each functionality.

## Configuration

The different scripts will read the configuration
The following environment variables must be defined:

### `config.sh`

node and network parameters required by cardano-cli.

### `params.sh`

Initial token asset derived from the environment variables defined above, minted tokens currency ID, and validity token asset derived from the minting policy.

### `sign_send.sh`

This script signs and submits the transaction to the chain. It takes a skey name and assumes its stored in the wallets folder.
Feel free to modify it to suit your security needs.

## Helper script to mint a token

You can use the script `demo/mint_fake_nft.sh` to mint an arbitrary token for locking/adding purposes.

## Initial setup

Before executing the lock and mint script, we need to generate validator and minting (.plutus) contracts, the contract address, and fractional asset currency id.  
Requires a list of extended public keys paths given in the `authorized_keys.txt` file and list of private keys paths in the `authorized_keys_s.txt` file. (see `demo/authorized_keys.txt` and `demo/authorized_keys_s.txt`)
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

(see `demo/init.sh` for usage)

## Lock token and initial mint

To lock the initial token and mint the fractional tokens, run:

```sh
lock_nft.sh {wallet} {initial amount to mint} {minimum signatures required} {initial token currency ID} {initial token name}
```

Where :
* `{wallet}` is the name of the `.addr` file in wallet folder that contains the address of the wallet holding the token,
* `{initial amount to mint}` is the number of fractional tokens to mint,
* `{minimum signatures required}` is the number of signatures (of wallets specified in `authorized_keys.txt`) that will be required later in `add_nft` and `mint_more` functions,
* `{initial token currency ID}` is the currency ID of the token to be locked,
* `{initial token name}` is the name (in plaintext) of the token to be locked,

You will be prompted to select the token and collateral utxo, and the transaction will be built, signed, and sent (using `sign_send.sh`).
The current datum will be saved in the `datum.json` file.

## Include additional tokens in the contract

To add a new token to the contract, it first must be approved by the authorized signatures (defined in the initial setup in the `authorized_keys.txt` file ), so the process is broken into two steps:

* Build a raw transaction
* Sign the transaction with enough approvers and send it

### Generate a transaction to be signed for adding a new token

Calling

```sh
add_nft.sh {token currency ID} {token name} {wallet name}
```

Will generate a shell script to be called with the required wallet names, in the form of `add_nft_cont.sh`, and will execute it automatically. This script will build the transaction into `tx.raw` file.

### Signing and sending the transaction to add a new token

Approvers can then sign the transaction with:

```sh
multisign_send.sh {wallet name 1} {wallet name 2} ...
```

## Mint more fractional tokens

To mint more fractional tokens, it first must be approved by the authorized signatures (defined in the initial setup in the `authorized_keys.txt` file ), so the process is broken into two steps:

* Generate the datum updating the total count of fractional tokens minted and build a raw transaction
* Sign the transaction with enough approvers and send it

### Generate a transaction to be signed for minting additional fractional tokens

Calling

```sh
mint_more.sh {extra amount to mint} {wallet name}
```

Will read the current datum and generate a new `datum.json` file updated with the total amount of fractional tokens minted.  
The script will also generate a shell script to be called with the required wallet names, in the form of `mint_more_cont.sh`, and will execute it automatically. This script will build the transaction into `tx.raw` file.

### Signing and sending the transaction to mint additional fractional tokens

Approvers can then sign the transaction with:

```sh
multisign_send.sh {wallet name 1} {wallet name 2} ...
```

## Return all tokens

To unlock all the tokens from the contract, the wallet has to burn all the fractional tokens.
Calling:

```sh
return_nft.sh {wallet} {total tokens to burn}
```

Will prompt you to select the UTxO containing the fractional tokens to burn from {wallet}, the UTxO that contains all the tokens in the contract, and the UTxO for collateral.  
It will sign and send the transaction, burning the fractional tokens and paying the tokens to the wallet.
