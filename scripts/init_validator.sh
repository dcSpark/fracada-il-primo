. exec_paths.sh
. params.sh

MIN_SIGS=2

echo "Backup old scripts"
cp validator.plutus validator.plutus.bk
cp validator-hash.txt validator-hash.txt.bk
cp minting.plutus minting.plutus.bk
cp currency-id.txt currency-id.txt.bk

#generate script
${SCRIPT_DUMP} ${NFT_CURRENCY} ${NFT_TOKEN} ${FRACT_TOKEN} 2 < authorized_keys.txt

$CARDANO_CLI address build --payment-script-file validator.plutus --testnet-magic $TESTNET_MAGIC_NUM --out-file wallets/validator.addr