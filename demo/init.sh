# CREATE DIRECTORIES IF NOT EXISTS
mkdir -p address
mkdir -p wallets
mkdir -p plutus

# LOAD CARDANO VARIABLES
. ./demo_config.sh

# GENERATE KEYS
. ./init-keys.sh

# INIT PLUTUS
. init_validator.sh