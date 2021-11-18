export NFT_CURRENCY=58e4abca68b2f61f8ec0a930016ab0817dfdc0d5183587f10fa501fb
export NFT_TOKEN=FakeNFTToken
export FRACT_CURRENCY=$(cat currency-id.txt)
export FRACT_TOKEN=FracadaSubtoken
export FRACT_TOKEN_HEX=$(echo $FRACT_TOKEN | xxd -p)