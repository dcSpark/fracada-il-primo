if [ -z "$1" ]
then
  read -p 'NFT currency id: ' NEW_NFT_CURRENCY
else
  echo 'NFT currency id: ' $1
  NEW_NFT_CURRENCY=$1
fi

if [ -z "$2" ]
then
  read -p 'NFT token name: ' NEW_NFT_TOKEN
else
  echo 'NFT token name: ' $2
  NEW_NFT_TOKEN=$2
fi
if [ -z "$3" ]
then
  read -p 'Wallet Name: ' SELECTED_WALLET_NAME
else
  echo 'Wallet Name: ' $3
  SELECTED_WALLET_NAME=$3
fi

SIGNING_WALLET=$SELECTED_WALLET_NAME
SELECTED_WALLET_ADDRESS=$(cat ./wallets/$SELECTED_WALLET_NAME.addr)

NEW_NFT_TOKEN_HEX=$(echo -n $NEW_NFT_TOKEN | xxd -p)
NEW_NFT_ASSET=$NEW_NFT_CURRENCY.$NEW_NFT_TOKEN_HEX

. config.sh
. functions.sh
. params.sh

section "Select NEW NFT UTxO"
getInputTx $SIGNING_WALLET
NEW_NFT_UTXO=$SELECTED_UTXO

section "Select main NFT UTxO"
getInputTx validator
NFT_UTXO=$SELECTED_UTXO
NFT_UTXO_LOVELACE=$SELECTED_UTXO_LOVELACE
NFT_UTXO_TOKENS=$SELECTED_UTXO_TOKENS

section "Select Collateral UTxO"
getInputTx $SIGNING_WALLET
COLLATERAL_TX=$SELECTED_UTXO

echo "building redeemer"
build-redeemer.sh ${NEW_NFT_CURRENCY} ${NEW_NFT_TOKEN}  < signaturefiles.txt

echo "add NFT"
$CARDANO_CLI transaction build \
--alonzo-era \
--$NETWORK_SELECTION \
--tx-in ${NFT_UTXO} \
--tx-in-script-file validator.plutus \
--tx-in-datum-file current-datum.json \
--tx-in-redeemer-file redeemer.json \
--tx-in ${COLLATERAL_TX} \
--tx-in ${NEW_NFT_UTXO} \
--tx-in-collateral ${COLLATERAL_TX} \
--tx-out "$(cat wallets/validator.addr) + ${NFT_UTXO_LOVELACE} + ${NFT_UTXO_TOKENS} + 1 $NEW_NFT_ASSET" \
--tx-out-datum-embed-file datum.json \
--change-address ${SELECTED_WALLET_ADDRESS} \
--protocol-params-file pparams.json \
--out-file tx.raw
