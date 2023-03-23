# LOAD CARDANO VARIABLES
. ./config.sh

# generate addresses
# main address
$CARDANO_CLI address key-gen \
--verification-key-file wallets/payment.vkey \
--signing-key-file wallets/payment.skey

$CARDANO_CLI stake-address key-gen \
--verification-key-file wallets/stake.vkey \
--signing-key-file wallets/stake.skey

$CARDANO_CLI address build \
--payment-verification-key-file wallets/payment.vkey \
--stake-verification-key-file wallets/stake.vkey \
--out-file wallets/payment.addr \
--$NETWORK_SELECTION

#fee address
$CARDANO_CLI address key-gen \
--verification-key-file wallets/fees.vkey \
--signing-key-file wallets/fees.skey \
--extended-key

$CARDANO_CLI stake-address key-gen \
--verification-key-file wallets/fees_stk.vkey \
--signing-key-file wallets/fees_stk.skey

$CARDANO_CLI address build \
--payment-verification-key-file wallets/fees.vkey \
--stake-verification-key-file wallets/fees_stk.vkey \
--out-file wallets/fees.addr \
--$NETWORK_SELECTION

[ -f "wallets/policy.script" ] && rm wallets/policy.script
touch wallets/policy.script
echo "{" >> wallets/policy.script
echo "  \"keyHash\": \"$(cardano-cli address key-hash --payment-verification-key-file wallets/payment.vkey)\"," >> wallets/policy.script
echo "  \"type\": \"sig\"" >> wallets/policy.script
echo "}" >> wallets/policy.script


# 3 signatures
$CARDANO_CLI address key-gen --verification-key-file wallets/key1.vkey --signing-key-file wallets/key1.skey --extended-key
$CARDANO_CLI address key-gen --verification-key-file wallets/key2.vkey --signing-key-file wallets/key2.skey --extended-key
$CARDANO_CLI address key-gen --verification-key-file wallets/key3.vkey --signing-key-file wallets/key3.skey --extended-key

[ -f "wallets/pubkeys.hash" ] && rm wallets/pubkeys.hash
while read line || [ -n "$line" ]; do
# reading each line
$CARDANO_CLI address key-hash --payment-verification-key-file $line >> wallets/pubkeys.hash
done < 'authorized_keys.txt'