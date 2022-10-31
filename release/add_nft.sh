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


CONT_FILENAME="add_nft_cont.sh"
[ -f "$CONT_FILENAME" ] && rm $CONT_FILENAME
echo "add NFT"
echo "$CARDANO_CLI transaction build \\" >> $CONT_FILENAME
echo "--$NETWORK_SELECTION \\" >> $CONT_FILENAME
echo "--tx-in ${NFT_UTXO} \\" >> $CONT_FILENAME
echo "--tx-in-script-file ./plutus/validator.plutus \\" >> $CONT_FILENAME
echo "--tx-in-datum-file datum.json \\" >> $CONT_FILENAME
echo "--tx-in-redeemer-file ../release/empty-redeemer.json \\" >> $CONT_FILENAME
echo "--tx-in-collateral ${COLLATERAL_TX} \\" >> $CONT_FILENAME
echo "--tx-in ${NEW_NFT_UTXO} \\" >> $CONT_FILENAME
echo "--tx-out \"$(cat wallets/validator.addr) + 3000000 + ${NFT_UTXO_TOKENS} + 1 $NEW_NFT_ASSET\" \\" >> $CONT_FILENAME
echo "--tx-out-datum-embed-file datum.json \\" >> $CONT_FILENAME
echo "--change-address ${SELECTED_WALLET_ADDRESS} \\" >> $CONT_FILENAME
echo "--protocol-params-file pparams.json \\" >> $CONT_FILENAME
echo "--out-file tx.raw \\" >> $CONT_FILENAME


mapfile -t < authorized_keys_s.txt
for i in "${MAPFILE[@]}";
  do
      echo "--required-signer $i \\" >> $CONT_FILENAME
  done

. $CONT_FILENAME

echo "Use \"multisign_send.sh\" to sign tx.raw with required signatures and send it"
chmod 755 $CONT_FILENAME