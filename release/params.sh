export FRACT_CURRENCY=$(cat ./plutus/currency-id.txt)

VALIDITY_TOKEN_HEX=$(echo -n 'FRACADA_VALIDITY' | xxd -p)
export VALIDITY_ASSET=$FRACT_CURRENCY.$VALIDITY_TOKEN_HEX