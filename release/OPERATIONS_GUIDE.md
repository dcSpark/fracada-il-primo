# Fracada il primo operations guide
This dApp expands on fracada, enabling adding more NFTs to the contract and mint more tokens after the intial minting.
Here we describe the steps to execute for each functionality.


## Configuration
The different scripts will read the confgiuration

### config.sh

node and network parameters

### params.sh

initial nft asset and factional asset derived from the environment variables:

### exec_paths.sh

paths to executable utilities

## Initial setup
Before executing the lock and mint script, we need to generate validator and minting (.plutus) contracts, the contract address, and fractional asset currency id.
The initial NFT to lock, the fraction token name, and the minimum number of signatures required will be read from the environment (see `demo/demo_params.sh`).
Requires a list of extended public keys paths given in the `authorized_keys.txt` file. (see `demo/authorized_keys.txt`)
The command is invoked with 
```
init_validator.sh
```

## Lock NFT and intial mint

lock_nft.sh {wallet} {initial amount to mint}


## Add extra NFT

```
add_nft.sh {nft currency id} {nft token name}
```

sign the generated datum and continue with:

```
continue_add_nft_{nft currency id}_{nft token name}.sh {wallet}
```

## Mint more fractional tokens

```
mint_more.sh {amount}
```

sign the generated datum

```
continue with continue_mint_{amount}.sh {wallet}
```

## Return all NFTs

```
return_nft.sh {wallet} {total tokens to burn}
```

## Signing a datum

```
sign {file to sign} {signing key file} {output file}
```
