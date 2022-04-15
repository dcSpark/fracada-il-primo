. config.sh
. functions.sh
. params.sh

if [ -z "$1" ]
then
  read -p 'Token Name:' TOKEN_NAME
else
  echo 'Token Name:' $1
  TOKEN_NAME=$1
fi

section "Select payment UTxO"
getInputTx payment
PAYMENT_UTXO=$SELECTED_UTXO

NEW_NFT_TOKEN_HEX=$(echo -n $TOKEN_NAME | xxd -p)
NEW_NFT_ASSET=$NFT_CURRENCY.$NEW_NFT_TOKEN_HEX

$CARDANO_CLI transaction build \
--tx-in $PAYMENT_UTXO \
--tx-out "$(cat wallets/payment.addr) + 1379280 + 1 ${NEW_NFT_ASSET}" \
--mint "1 ${NEW_NFT_ASSET}" \
--mint-script-file ./wallets/policy.script \
--change-address $(cat wallets/payment.addr) \
--$NETWORK_SELECTION \
--protocol-params-file pparams.json \
--out-file tx.raw

. sign_send.sh payment