. config.sh
$CARDANO_CLI transaction sign \
--tx-body-file tx.raw \
--signing-key-file ./address/$1.skey \
--signing-key-file ./address/fees.skey \
--$NETWORK_SELECTION \
--out-file tx.signed \

$CARDANO_CLI transaction submit --tx-file tx.signed --$NETWORK_SELECTION