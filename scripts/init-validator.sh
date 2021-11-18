. exec_paths.sh
. params.sh
MIN_SIGS=2

#generate script
${SCRIPT_DUMP} ${NFT_CURRENCY} ${NFT_TOKEN} ${FRACT_TOKEN} 2 < authorized_keys.txt

$CARDANO_CLI address build --payment-script-file validator.plutus --testnet-magic $TESTNET_MAGIC_NUM --out-file wallet/validator.addr