. config.sh
. exec_paths.sh
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
getInputTx payment2
PAYMENT_UTXO=$SELECTED_UTXO

section "Select Collateral UTxO"
getInputTx payment2
COLLATERAL_TX=$SELECTED_UTXO


NEW_NFT_TOKEN_HEX=$(echo -n $TOKEN_NAME | xxd -p)
NEW_NFT_ASSET=$NFT_CURRENCY.$NEW_NFT_TOKEN_HEX

$CARDANO_CLI transaction build \
--alonzo-era \
--tx-in $PAYMENT_UTXO \
--tx-out "$(cat wallets/payment2.addr) + 1379280 + 1 ${NEW_NFT_ASSET}" \
--mint "1 ${NEW_NFT_ASSET}" \
--mint-script-file ../../fracada/freemint.plutus \
--mint-redeemer-value [] \
--tx-in-collateral $COLLATERAL_TX \
--change-address $(cat wallets/payment2.addr) \
--testnet-magic $TESTNET_MAGIC_NUM \
--protocol-params-file pparams.json \
--out-file tx.raw

. sign_send.sh payment2