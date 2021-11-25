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
  read -p 'NFT currency id: ' NEW_NFT_CURRENCY
else
  echo 'NFT currency id: ' $2
  NEW_NFT_CURRENCY=$2
fi

if [ -z "$3" ]
then
  read -p 'NFT token name: ' NEW_NFT_TOKEN
else
  echo 'NFT token name: ' $3
  NEW_NFT_TOKEN=$3
fi
NEW_NFT_TOKEN_HEX=$(echo -n $NEW_NFT_TOKEN | xxd -p)
NEW_NFT_ASSET=$NEW_NFT_CURRENCY.$NEW_NFT_TOKEN_HEX

. config.sh
. exec_paths.sh
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
getInputTx fees
COLLATERAL_TX=$SELECTED_UTXO

echo "build datum"
mv datum.txt current-datum.txt
${BUILD_DATUM} add-nft current-datum.txt ${NEW_NFT_CURRENCY} ${NEW_NFT_TOKEN}

echo "sign datum"
${SIGN} datum-hash.txt address/key1.skey d1.sign
${SIGN} datum-hash.txt address/key2.skey d2.sign
${SIGN} datum-hash.txt address/key3.skey d3.sign

echo d1.sign > signaturefiles.txt
echo d2.sign >> signaturefiles.txt
echo d3.sign >> signaturefiles.txt

echo "build redeemer"
${BUILD_REDEEMER} ${NEW_NFT_CURRENCY} ${NEW_NFT_TOKEN}  < signaturefiles.txt

echo "add NFT"
$CARDANO_CLI transaction build \
--alonzo-era \
--testnet-magic $TESTNET_MAGIC_NUM \
--tx-in ${NFT_UTXO} \
--tx-in-script-file validator.plutus \
--tx-in-datum-file current-datum.txt \
--tx-in-redeemer-file redeemer.txt \
--tx-in ${COLLATERAL_TX} \
--tx-in ${NEW_NFT_UTXO} \
--tx-in-collateral ${COLLATERAL_TX} \
--tx-out "$(cat wallets/validator.addr) + ${NFT_UTXO_LOVELACE} + ${NFT_UTXO_TOKENS} + 1 $NEW_NFT_ASSET" \
--tx-out-datum-embed-file datum.txt \
--change-address ${SELECTED_WALLET_ADDRESS} \
--protocol-params-file pparams.json \
--out-file tx.raw

. sign_send.sh $SIGNING_WALLET


if [ $? -eq 0 ] 
then 
  echo "Success" 
else 
  echo "Failed, restoring datum state" >&2
  mv current-datum.txt datum.txt   
fi