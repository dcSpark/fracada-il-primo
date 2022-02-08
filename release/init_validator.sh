. params.sh

echo "Backup old scripts"
cp validator.plutus validator.plutus.bk
cp validator-hash.txt validator-hash.txt.bk
cp minting.plutus minting.plutus.bk
cp currency-id.txt currency-id.txt.bk

#generate script
. script-dump.sh ${NFT_CURRENCY} ${NFT_TOKEN} ${FRACT_TOKEN} ${MIN_SIGS} < authorized_keys.txt
#Build validator's address
$CARDANO_CLI address build --payment-script-file validator.plutus --$NETWORK_SELECTION --out-file wallets/validator.addr

#Query protocol parameters
${CARDANO_CLI} query protocol-parameters --testnet-magic  $TESTNET_MAGIC_NUM --out-file pparams.json
