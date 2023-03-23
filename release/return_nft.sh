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
  read -p 'Amount to burn: ' FRACT_TOKENS_AMOUNT
else
  echo 'Amount to burn:' $2
  FRACT_TOKENS_AMOUNT=$2
fi

section "Select fractional tokens UTxO"
getInputTx $SELECTED_WALLET_NAME
FRACTIONS_UTXO=$SELECTED_UTXO

section "Select NFT tokens UTxO"
getInputTx validator
NFT_UTXO=$SELECTED_UTXO
NFT_UTXO_LOVELACE=$SELECTED_UTXO_LOVELACE
NFT_UTXO_TOKENS=$UTXO_TOKENS_WITHOUT_VALIDITY

section "Select Collateral UTxO"
getInputTx $SIGNING_WALLET
COLLATERAL_TX=$SELECTED_UTXO

#build redeemer
. build-redeemer.sh 'Burn'
loadFractionTokenName

echo "pay NFT back and burn fraction tokens"
$CARDANO_CLI transaction build \
--$NETWORK_SELECTION \
--tx-in ${FRACTIONS_UTXO} \
--tx-in ${NFT_UTXO} \
--tx-in-script-file ./plutus/validator.plutus \
--tx-in-datum-file datum.json \
--tx-in-redeemer-file ../release/empty-redeemer.json \
--tx-in ${COLLATERAL_TX} \
--tx-in-collateral ${COLLATERAL_TX} \
--tx-out "${SELECTED_WALLET_ADDRESS} + 2000000 + ${NFT_UTXO_TOKENS}" \
--mint "-${FRACT_TOKENS_AMOUNT} ${FRACT_ASSET} + -1 ${VALIDITY_ASSET}" \
--mint-script-file ./plutus/minting.plutus \
--mint-redeemer-file redeemer.json \
--change-address ${SELECTED_WALLET_ADDRESS} \
--protocol-params-file pparams.json \
--out-file tx.raw

. sign_send.sh $SIGNING_WALLET