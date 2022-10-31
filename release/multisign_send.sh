. config.sh

if [ -z "$1" ]
then
  read -r -p 'Enter all wallet names separated by space: ' -a SELECTED_WALLETS
else
  echo 'Wallet Name: ' $@
  declare -a SELECTED_WALLETS=("$@")
fi

CONT_FILENAME="multisign_send_cont.sh"
[ -f "$CONT_FILENAME" ] && rm $CONT_FILENAME
echo "$CARDANO_CLI transaction sign \\" >> $CONT_FILENAME
echo "--tx-body-file tx.raw \\" >> $CONT_FILENAME
echo "--$NETWORK_SELECTION \\" >> $CONT_FILENAME
echo "--out-file tx.signed \\" >> $CONT_FILENAME

for i in "${SELECTED_WALLETS[@]}";
  do
      echo "--signing-key-file ./wallets/$i.skey \\" >> $CONT_FILENAME
  done

. $CONT_FILENAME

$CARDANO_CLI transaction submit --tx-file tx.signed --$NETWORK_SELECTION

