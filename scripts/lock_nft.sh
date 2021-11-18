. config.sh
. exec_paths.sh
. functions.sh
. params.sh

section "Select NFT UTxO"
getInputTx 
NFT_UTXO=$SELECTED_UTXO
SIGNING_WALLET=$SELECTED_WALLET_NAME
SELECTED_WALLET_ADDRESS=$SELECTED_WALLET_ADDR

section "Select Collateral UTxO"
getInputTx fees
COLLATERAL_TX=$SELECTED_UTXO

INIAL_FRACT_TOKENS_AMOUNT=20

#build datum
${BUILD_DATUM} new ${FRACT_CURRENCY} ${FRACT_TOKEN} ${INIAL_FRACT_TOKENS_AMOUNT} ${NFT_CURRENCY} ${NFT_TOKEN} 

# pay NFT and mint fraction tokens
$CARDANO_CLI transaction build \
--alonzo-era \
--testnet-magic $TESTNET_MAGIC_NUM \
--tx-in ${NFT_UTXO} \
--tx-in ${COLLATERAL_TX} \
--tx-in-collateral ${COLLATERAL_TX} \
--tx-out "$(cat wallets/validator.addr) + 1724100 + 1 ${NFT_CURRENCY}.${NFT_TOKEN}" \
--tx-out-datum-hash $(cat datum-hash.txt) \
--tx-out "${SELECTED_WALLET_ADDRESS} + 1620654 + ${INIAL_FRACT_TOKENS_AMOUNT} ${FRACT_CURRENCY}.${FRACT_TOKEN}" \
--mint "${INIAL_FRACT_TOKENS_AMOUNT}  ${FRACT_CURRENCY}.${FRACT_TOKEN}" \
--mint-script-file minting.plutus \
--mint-redeemer-value {} \
--change-address ${SELECTED_WALLET_ADDRESS} \
--protocol-params-file pparams.json \
--out-file tx.raw

. sign_send.sh