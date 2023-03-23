function getInputTx() {
	BALANCE_FILE=/tmp/walletBalances.txt
	rm -f $BALANCE_FILE
	if [ -z "$1" ]
	then
		read -p 'Wallet Name: ' SELECTED_WALLET_NAME
	else
		echo 'Wallet Name: ' $1
		SELECTED_WALLET_NAME=$1
	fi
	balance.sh $SELECTED_WALLET_NAME > $BALANCE_FILE
	SELECTED_WALLET_ADDR=$(cat ./wallets/$SELECTED_WALLET_NAME.addr)
	cat $BALANCE_FILE
	read -p 'TX row number: ' TMP
	TX_ROW_NUM="$(($TMP+2))"
	TX_ROW=$(sed "${TX_ROW_NUM}q;d" $BALANCE_FILE)
	SELECTED_UTXO_HASH="$(echo $TX_ROW | awk '{ print $2 }')"
	SELECTED_UTXO_INDEX="$(echo $TX_ROW | awk '{ print $3 }')"
	SELECTED_UTXO=$SELECTED_UTXO_HASH'#'$SELECTED_UTXO_INDEX
	SELECTED_UTXO_LOVELACE=$(echo $TX_ROW | awk '{ print $4 }')
	SELECTED_UTXO_TOKENS=$(echo $TX_ROW | awk 'BEGIN { FS = "lovelace \\+ | \\+ TxOutDatumHash" } ; { print $2 }')
	UTXO_TOKENS_WITHOUT_VALIDITY=${SELECTED_UTXO_TOKENS/1 $VALIDITY_ASSET + /}
	UTXO_TOKENS_WITHOUT_VALIDITY=${UTXO_TOKENS_WITHOUT_VALIDITY/ + 1 $VALIDITY_ASSET/}
}

function loadFractionTokenName() {
	FRACT_TOKEN_NAME=`cat fraction-tokenname.txt`
	# remove leading and trailing quotes
	FRACT_TOKEN_NAME="${FRACT_TOKEN_NAME%\"}"
	FRACT_TOKEN_NAME="${FRACT_TOKEN_NAME#\"}"
	FRACT_ASSET=$FRACT_CURRENCY.$FRACT_TOKEN_NAME
}

walletAddress() {
	WALLET_ADDRESS=$(cat ./wallets/$1.addr)
}

setDatumHash() {
	DATUM_HASH=$(cardano-cli transaction hash-script-data --script-data-value $DATUM_VALUE)
	#return $(cardano-cli transaction hash-script-data --script-data-value $1)
}

getScriptAddress() {
	SCRIPT_ADDRESS=$(cardano-cli address build --payment-script-file ./scripts/$1.plutus --$NETWORK_SELECTION)
        echo $SCRIPT_ADDRESS > ./wallets/$1.addr
}

function section {
  echo "============================================================================================"
  echo $1
  echo "============================================================================================"
}

function removeTxFiles() {
  rm -f tx.raw
  rm -f tx.signed
}

