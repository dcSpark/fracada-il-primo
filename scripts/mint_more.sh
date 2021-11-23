if [ -z "$1" ]
then
  read -p 'Wallet Name: ' SELECTED_WALLET_NAME
else
  echo 'Wallet Name: ' $1
  SELECTED_WALLET_NAME=$1
fi
SELECTED_WALLET_ADDRESS=$(cat ./wallets/$SELECTED_WALLET_NAME.addr)

if [ -z "$2" ]
then
  read -p 'Amount to mint: ' MORE_TOKENS_AMOUNT
else
  echo 'Tokens to mint: ' $2
  MORE_TOKENS_AMOUNT=$2
fi

. config.sh
. exec_paths.sh
. functions.sh
. params.sh

section "Select NFT UTxO"
getInputTx validator
NFT_UTXO=$SELECTED_UTXO
NFT_UTXO_LOVELACE=$SELECTED_UTXO_LOVELACE
NFT_UTXO_TOKENS=$SELECTED_UTXO_TOKENS

section "Select Collateral UTxO"
getInputTx fees
COLLATERAL_TX=$SELECTED_UTXO

echo "build datum"
mv datum.txt current-datum.txt
${BUILD_DATUM} mint-more current-datum.txt ${MORE_TOKENS_AMOUNT} 

echo "sign datum"
${SIGN} datum-hash.txt address/key1.skey d1.sign
${SIGN} datum-hash.txt address/key2.skey d2.sign
${SIGN} datum-hash.txt address/key3.skey d3.sign

echo d1.sign > signaturefiles.txt
echo d2.sign >> signaturefiles.txt
echo d3.sign >> signaturefiles.txt

echo "build redeemer"
${BUILD_REDEEMER} ${FRACT_CURRENCY} ${FRACT_TOKEN}  < signaturefiles.txt

echo "mint fraction tokens"
$CARDANO_CLI transaction build \
--alonzo-era \
--testnet-magic $TESTNET_MAGIC_NUM \
--tx-in ${NFT_UTXO} \
--tx-in-script-file validator.plutus \
--tx-in-datum-file current-datum.txt \
--tx-in-redeemer-file redeemer.txt \
--tx-in ${COLLATERAL_TX} \
--tx-in-collateral ${COLLATERAL_TX} \
--tx-out "$(cat wallets/validator.addr) + ${NFT_UTXO_LOVELACE} + ${NFT_UTXO_TOKENS}" \
--tx-out-datum-embed-file datum.txt \
--tx-out "${SELECTED_WALLET_ADDRESS} + 1620654 + ${MORE_TOKENS_AMOUNT} ${FRACT_CURRENCY}.${FRACT_TOKEN_HEX}" \
--mint "${MORE_TOKENS_AMOUNT} ${FRACT_CURRENCY}.${FRACT_TOKEN_HEX}" \
--mint-script-file minting.plutus \
--mint-redeemer-file redeemer.txt \
--change-address ${SELECTED_WALLET_ADDRESS} \
--protocol-params-file pparams.json \
--out-file tx.raw

. sign_send.sh