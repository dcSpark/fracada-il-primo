if [ -z "$1" ]
then
  read -p 'Amount to mint: ' MORE_TOKENS_AMOUNT
else
  echo 'Tokens to mint: ' $1
  MORE_TOKENS_AMOUNT=$1
fi

. params.sh

echo "Building datum"
mv datum.json current-datum.json
. build-datum.sh mint-more current-datum.json ${MORE_TOKENS_AMOUNT} 

echo "mint_more_cont.sh ${MORE_TOKENS_AMOUNT} $1" > continue_mint_${MORE_TOKENS_AMOUNT}.sh
echo "Datum for adding ${MORE_TOKENS_AMOUNT} tokens generated as \"datum.json\", get the required signatures and execute \"continue_mint_${MORE_TOKENS_AMOUNT}.sh <wallet to use>\" "