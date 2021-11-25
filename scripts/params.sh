export NFT_CURRENCY="58e4abca68b2f61f8ec0a930016ab0817dfdc0d5183587f10fa501fb"
export NFT_TOKEN="FakeNFTTokenB"
NFT_TOKEN_HEX=$(echo -n $NFT_TOKEN | xxd -p)
export NFT_ASSET=$NFT_CURRENCY.$NFT_TOKEN_HEX
export FRACT_CURRENCY=$(cat currency-id.txt)
export FRACT_TOKEN="FracadaSubtokenB"
FRACT_TOKEN_HEX=$(echo -n $FRACT_TOKEN | xxd -p)
export FRACT_ASSET=$FRACT_CURRENCY.$FRACT_TOKEN_HEX