if [ -z "$1" ]
then
  read -p 'Amount to mint: ' MORE_TOKENS_AMOUNT
else
  echo 'Tokens to mint: ' $1
  MORE_TOKENS_AMOUNT=$1
fi

if [ -z "$2" ]
then
  read -p 'Wallet Name: ' SELECTED_WALLET_NAME
else
  echo 'Wallet Name: ' $2
  SELECTED_WALLET_NAME=$2
fi
SIGNING_WALLET=$SELECTED_WALLET_NAME
SELECTED_WALLET_ADDRESS=$(cat ./wallets/$SELECTED_WALLET_NAME.addr)

. config.sh
. functions.sh
. params.sh

section "Select NFT UTxO"
getInputTx validator
NFT_UTXO=$SELECTED_UTXO
NFT_UTXO_LOVELACE=$SELECTED_UTXO_LOVELACE
NFT_UTXO_TOKENS=$SELECTED_UTXO_TOKENS

section "Select Collateral UTxO"
getInputTx $SIGNING_WALLET
COLLATERAL_TX=$SELECTED_UTXO

echo "Building redeemer"
. build-redeemer.sh 'MintMoreFractions'


echo "Building datum"
mv datum.json current-datum.json
. build-datum.sh mint-more current-datum.json ${MORE_TOKENS_AMOUNT}

loadFractionTokenName

CONT_FILENAME="mint_more_cont.sh"
[ -f "$CONT_FILENAME" ] && rm $CONT_FILENAME
echo "Mint fraction tokens"
echo "$CARDANO_CLI transaction build \\" >> $CONT_FILENAME
echo "--alonzo-era \\" >> $CONT_FILENAME
echo "--$NETWORK_SELECTION \\" >> $CONT_FILENAME
echo "--tx-in ${NFT_UTXO} \\" >> $CONT_FILENAME
echo "--tx-in-script-file ./plutus/validator.plutus \\" >> $CONT_FILENAME
echo "--tx-in-datum-file current-datum.json \\" >> $CONT_FILENAME
echo "--tx-in-redeemer-file ../release/empty-redeemer.json \\" >> $CONT_FILENAME
echo "--tx-in ${COLLATERAL_TX} \\" >> $CONT_FILENAME
echo "--tx-in-collateral ${COLLATERAL_TX} \\" >> $CONT_FILENAME
echo "--tx-out \"$(cat wallets/validator.addr) + ${NFT_UTXO_LOVELACE} + ${NFT_UTXO_TOKENS}\" \\" >> $CONT_FILENAME
echo "--tx-out-datum-embed-file datum.json \\" >> $CONT_FILENAME
echo "--tx-out \"${SELECTED_WALLET_ADDRESS} + 1620654 + ${MORE_TOKENS_AMOUNT} ${FRACT_ASSET}\" \\" >> $CONT_FILENAME
echo "--mint \"${MORE_TOKENS_AMOUNT} ${FRACT_ASSET}\" \\" >> $CONT_FILENAME
echo "--mint-script-file ./plutus/minting.plutus \\" >> $CONT_FILENAME
echo "--mint-redeemer-file redeemer.json \\" >> $CONT_FILENAME
echo "--change-address ${SELECTED_WALLET_ADDRESS} \\" >> $CONT_FILENAME
echo "--protocol-params-file pparams.json \\" >> $CONT_FILENAME
echo "--out-file tx.raw \\" >> $CONT_FILENAME

mapfile -t < authorized_keys_s.txt
for i in "${MAPFILE[@]}";
  do
      echo "--required-signer $i \\" >> $CONT_FILENAME
  done
echo 'running cont'
. $CONT_FILENAME

echo "Use \"multisign_send.sh\" to sign tx.raw with required signatures and send it"
chmod 755 $CONT_FILENAME