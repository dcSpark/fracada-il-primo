. config.sh
$CARDANO_CLI transaction sign \
--tx-body-file tx.raw \
--signing-key-file ./address/${SIGNING_WALLET}.skey \
--signing-key-file ./address/fees.skey \
--testnet-magic $TESTNET_MAGIC_NUM \
--out-file tx.signed \

$CARDANO_CLI transaction submit --tx-file tx.signed --testnet-magic $TESTNET_MAGIC_NUM