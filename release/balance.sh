. config.sh
$CARDANO_CLI query utxo --address $(cat ./wallets/$1.addr) --$NETWORK_SELECTION | nl -v-1

