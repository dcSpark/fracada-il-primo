NFT_TOKEN_HEX=$(echo -n $NFT_TOKEN | xxd -p)
export NFT_ASSET=$NFT_CURRENCY.$NFT_TOKEN_HEX

export FRACT_CURRENCY=$(cat ./plutus/currency-id.txt)
FRACT_TOKEN_HEX=$(echo -n $FRACT_TOKEN | xxd -p)
export FRACT_ASSET=$FRACT_CURRENCY.$FRACT_TOKEN_HEX