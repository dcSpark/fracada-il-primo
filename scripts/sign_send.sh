$CARDANO_CLI transaction sign \
--tx-body-file tx.build \
--signing-key-file ./wallets/${SIGNING_WALLET}.skey \
--testnet-magic $TESTNET_MAGIC_NUM \
--out-file tx.signed \

$CARDANO_CLI transaction submit --tx-file tx.signed --testnet-magic $TESTNET_MAGIC_NUM