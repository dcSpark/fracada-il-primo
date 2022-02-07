. config.sh
. functions.sh
. params.sh

if [ -z "$1" ]
then
  read -p 'Wallet Name: ' SELECTED_WALLET_NAME
else
  echo 'Wallet Name: ' $1
  SELECTED_WALLET_NAME=$1
fi
SIGNING_WALLET=$SELECTED_WALLET_NAME
SELECTED_WALLET_ADDRESS=$(cat ./wallets/$SELECTED_WALLET_NAME.addr)

if [ -z "$2" ]
then
  read -p 'Inital amount to mint: ' INITIAL_FRACT_TOKENS_AMOUNT
else
  echo 'Inital amount to mint:' $2
  INITIAL_FRACT_TOKENS_AMOUNT=$2
fi

section "Select NFT UTxO"
getInputTx $SELECTED_WALLET_NAME
NFT_UTXO=$SELECTED_UTXO

section "Select Collateral UTxO"
getInputTx $SIGNING_WALLET
COLLATERAL_TX=$SELECTED_UTXO


#build datum
. build-datum.sh new ${FRACT_CURRENCY} ${FRACT_TOKEN} ${INITIAL_FRACT_TOKENS_AMOUNT} ${NFT_CURRENCY} ${NFT_TOKEN} 

# pay NFT and mint fraction tokens
$CARDANO_CLI transaction build \
--alonzo-era \
--$NETWORK_SELECTION \
--tx-in ${NFT_UTXO} \
--tx-in ${COLLATERAL_TX} \
--tx-in-collateral ${COLLATERAL_TX} \
--tx-out "$(cat wallets/validator.addr) + 1724100 + 1 ${NFT_ASSET}" \
--tx-out-datum-hash $(cat datum-hash.txt) \
--tx-out "${SELECTED_WALLET_ADDRESS} + 1620654 + ${INITIAL_FRACT_TOKENS_AMOUNT} ${FRACT_ASSET}" \
--mint "${INITIAL_FRACT_TOKENS_AMOUNT} ${FRACT_ASSET}" \
--mint-script-file minting.plutus \
--mint-redeemer-value {} \
--change-address ${SELECTED_WALLET_ADDRESS} \
--protocol-params-file pparams.json \
--out-file tx.raw

. sign_send.sh $SIGNING_WALLET