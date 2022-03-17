# CREATE DIRECTORIES IF NOT EXISTS
mkdir -p wallets
mkdir -p plutus

# GENERATE KEYS
. ./init-keys.sh

# LOAD CARDANO VARIABLES
. ./demo_config.sh

# INIT PLUTUS
. init_validator.sh