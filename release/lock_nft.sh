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
  echo 'Inital amount to mint: ' $2
  INITIAL_FRACT_TOKENS_AMOUNT=$2
fi

if [ -z "$3" ]
then
  read -p 'Minimum signatures required: ' MIN_SIGS_REQUIRED
else
  echo 'Minimum signatures required: ' $3
  MIN_SIGS_REQUIRED=$3
fi

read -r -p 'Enter authorized PubKeyHashes separated by space: ' -a AUTH_PUB_KEYS


section "Select NFT UTxO"
getInputTx $SELECTED_WALLET_NAME
NFT_UTXO=$SELECTED_UTXO

section "Select Collateral UTxO"
getInputTx $SIGNING_WALLET
COLLATERAL_TX=$SELECTED_UTXO


#build datum
. build-datum.sh new ${FRACT_CURRENCY} ${FRACT_TOKEN} ${INITIAL_FRACT_TOKENS_AMOUNT} ${MIN_SIGS_REQUIRED} ${AUTH_PUB_KEYS[@]}

# pay NFT and mint fraction tokens
$CARDANO_CLI transaction build \
--$NETWORK_SELECTION \
--tx-in ${NFT_UTXO} \
--tx-in-collateral ${COLLATERAL_TX} \
--tx-out "$(cat wallets/validator.addr) + ${MIN_ADA} + 1 ${NFT_ASSET}" \
--tx-out-datum-embed-file datum.json \
--tx-out "${SELECTED_WALLET_ADDRESS} + ${MIN_ADA} + ${INITIAL_FRACT_TOKENS_AMOUNT} ${FRACT_ASSET}" \
--mint "${INITIAL_FRACT_TOKENS_AMOUNT} ${FRACT_ASSET}" \
--minting-script-file ./plutus/minting.plutus \
--mint-redeemer-value {} \
--change-address ${SELECTED_WALLET_ADDRESS} \
--protocol-params-file pparams.json \
--out-file tx.raw

. sign_send.sh $SIGNING_WALLET