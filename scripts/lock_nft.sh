. exec_paths.sh
section "Select NFT UTxO"
getInputTx 
NFT_UTXO=$SELECTED_UTXO
SIGNING_WALLET=$SELECTED_WALLET_NAME
SELECTED_WALLET_ADDRESS =$SELECTED_WALLET_ADDR

section "Select Collateral UTxO"
getInputTx fees
COLLATERAL_TX=$SELECTED_UTXO

INIAL_FRAC_TOKENS_AMOUNT=20

#build datum
${BUILD_DATUM} ${FRAC_CURRENCY} ${FRAC_TOKEN} ${INIAL_FRAC_TOKENS_AMOUNT} ${NFT_CURRENCY} ${NFT_TOKEN} 

# pay NFT and mint fraction tokens
$CARDANO_CLI transaction build \
--alonzo-era \
--testnet-magic $TESTNET_MAGIC_NUM \
--tx-in ${NFT_UTXO} \
--tx-in ${COLLATERAL_TX} \
--tx-in-collateral ${COLLATERAL_TX} \
--tx-out "$(cat address/validator.addr) + 1724100 + 1 ${NFT_CURRENCY}.${NFT_TOKEN}" \
--tx-out-datum-hash $(cat datum-hash.init.txt) \
--tx-out "$(cat address/payment2.addr) + 1620654 + ${INIAL_FRAC_TOKENS_AMOUNT} ${FRAC_CURRENCY}.${FRAC_TOKEN}" \
--mint "${INIAL_FRAC_TOKENS_AMOUNT}  ${FRAC_CURRENCY}.${FRAC_TOKEN}" \
--mint-script-file minting.plutus \
--mint-redeemer-value {} \
--change-address ${SELECTED_WALLET_ADDRESS} \
--protocol-params-file pparams.json \
--out-file tx.raw

. sign_send.sh