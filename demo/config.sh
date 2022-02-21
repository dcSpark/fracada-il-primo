export CARDANO_CLI=../../cardano-node/dist-newstyle/build/x86_64-linux/ghc-8.10.5/cardano-cli-1.31.0/x/cardano-cli/build/cardano-cli/cardano-cli

export TESTNET_MAGIC_NUM=1097911063
export CARDANO_NODE_SOCKET_PATH=~/encrypted/dcSpark/cardano-node/mainnet/db/node.socket

# replace with 'mainnet' to use the main network
export NETWORK_SELECTION="testnet-magic $TESTNET_MAGIC_NUM"
