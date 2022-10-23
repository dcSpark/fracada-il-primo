. config.sh
. functions.sh
. params.sh

if [ -z "$1" ]
then
  read -p 'NFT currency id: ' NEW_NFT_CURRENCY
else
  echo 'NFT currency id: ' $1
  NEW_NFT_CURRENCY=$1
fi

if [ -z "$2" ]
then
  read -p 'NFT token name: ' NEW_NFT_TOKEN
else
  echo 'NFT token name: ' $2
  NEW_NFT_TOKEN=$2
fi


echo "add_nft_cont.sh ${NEW_NFT_CURRENCY} ${NEW_NFT_TOKEN}" > continue_add_nft_${NEW_NFT_CURRENCY}_${NEW_NFT_TOKEN}.sh
echo "Get the required signatures and execute \"continue_add_nft_${NEW_NFT_CURRENCY}_${NEW_NFT_TOKEN}.sh <wallet to use>\" "
chmod 755 continue_add_nft_${NEW_NFT_CURRENCY}_${NEW_NFT_TOKEN}.sh