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
. exec_paths.sh
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
${BUILD_REDEEMER} ${FRACT_CURRENCY} ${FRACT_TOKEN}  < signaturefiles.txt

echo "Mint fraction tokens"
$CARDANO_CLI transaction build \
--alonzo-era \
--testnet-magic $TESTNET_MAGIC_NUM \
--tx-in ${NFT_UTXO} \
--tx-in-script-file validator.plutus \
--tx-in-datum-file current-datum.json \
--tx-in-redeemer-file redeemer.json \
--tx-in ${COLLATERAL_TX} \
--tx-in-collateral ${COLLATERAL_TX} \
--tx-out "$(cat wallets/validator.addr) + ${NFT_UTXO_LOVELACE} + ${NFT_UTXO_TOKENS}" \
--tx-out-datum-embed-file datum.json \
--tx-out "${SELECTED_WALLET_ADDRESS} + 1620654 + ${MORE_TOKENS_AMOUNT} ${FRACT_ASSET}" \
--mint "${MORE_TOKENS_AMOUNT} ${FRACT_ASSET}" \
--mint-script-file minting.plutus \
--mint-redeemer-value {} \
--change-address ${SELECTED_WALLET_ADDRESS} \
--protocol-params-file pparams.json \
--out-file tx.raw

. sign_send.sh $SIGNING_WALLET

