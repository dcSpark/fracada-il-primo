export PATH=../release:../release/bin:$PATH
export CARDANO_CLI=cardano-cli

export TESTNET_MAGIC_NUM=2
export CARDANO_NODE_SOCKET_PATH=~/cardano/node.socket

# replace with 'mainnet' to use the main network
export NETWORK_SELECTION="testnet-magic $TESTNET_MAGIC_NUM"

export MIN_ADA=1758582