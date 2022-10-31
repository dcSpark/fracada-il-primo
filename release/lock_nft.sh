export PATH=../demo:$PATH
# LOAD CARDANO VARIABLES
. ../demo/demo_params.sh
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

if [ -z "$4" ]
then
  read -p 'Currency ID of the initial token to be locked: ' NFT_CURRENCY
else
  echo 'Currency ID of the initial token to be locked: ' $4
  NFT_CURRENCY=$4
fi

if [ -z "$5" ]
then
  read -p 'Name of the initial token to be locked: ' NFT_TOKEN_NAME
else
  echo 'Name of the initial token to be locked: ' $5
  NFT_TOKEN_NAME=$5
fi

NFT_TOKEN_HEX=$(echo -n $NFT_TOKEN_NAME | xxd -p)
NFT_ASSET=$NFT_CURRENCY.$NFT_TOKEN_HEX

section "Select NFT UTxO"
getInputTx $SELECTED_WALLET_NAME
NFT_UTXO=$SELECTED_UTXO
NFT_UTXO_HASH=$SELECTED_UTXO_HASH
NFT_UTXO_INDEX=$SELECTED_UTXO_INDEX

section "Select Collateral UTxO"
getInputTx $SIGNING_WALLET
COLLATERAL_TX=$SELECTED_UTXO

#build fraction token name
. build-fraction-tokenname.sh ${NFT_UTXO_HASH} ${NFT_UTXO_INDEX}
loadFractionTokenName

#build datum
mapfile -t < wallets/pubkeys.hash
. build-datum.sh new ${FRACT_CURRENCY} ${NFT_UTXO_HASH} ${NFT_UTXO_INDEX} ${INITIAL_FRACT_TOKENS_AMOUNT} ${MIN_SIGS_REQUIRED} ${MAPFILE[@]}

#build redeemer
. build-redeemer.sh 'InitialMint' ${NFT_UTXO_HASH} ${NFT_UTXO_INDEX}


# pay NFT and mint fraction tokens
$CARDANO_CLI transaction build \
--$NETWORK_SELECTION \
--tx-in ${NFT_UTXO} \
--tx-in-collateral ${COLLATERAL_TX} \
--tx-out "$(cat wallets/validator.addr) + ${MIN_ADA} + 1 ${NFT_ASSET} + 1 ${VALIDITY_ASSET}" \
--tx-out-datum-embed-file datum.json \
--tx-out "${SELECTED_WALLET_ADDRESS} + ${MIN_ADA} + ${INITIAL_FRACT_TOKENS_AMOUNT} ${FRACT_ASSET}" \
--mint "${INITIAL_FRACT_TOKENS_AMOUNT} ${FRACT_ASSET} + 1 ${VALIDITY_ASSET}" \
--minting-script-file ./plutus/minting.plutus \
--mint-redeemer-file redeemer.json \
--change-address ${SELECTED_WALLET_ADDRESS} \
--protocol-params-file pparams.json \
--out-file tx.raw

. sign_send.sh $SIGNING_WALLET