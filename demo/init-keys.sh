# generate addresses
# main address
$CARDANO_CLI address key-gen \
--verification-key-file address/payment2.vkey \
--signing-key-file address/payment2.skey

$CARDANO_CLI stake-address key-gen \
--verification-key-file address/stake2.vkey \
--signing-key-file address/stake2.skey

$CARDANO_CLI address build \
--payment-verification-key-file address/payment2.vkey \
--stake-verification-key-file address/stake2.vkey \
--out-file address/payment2.addr \
--$NETWORK_SELECTION

#fee address
$CARDANO_CLI address key-gen \
--verification-key-file address/fees.vkey \
--signing-key-file address/fees.skey \
--extended-key

$CARDANO_CLI stake-address key-gen \
--verification-key-file address/fees_stk.vkey \
--signing-key-file address/fees_stk.skey 

$CARDANO_CLI address build \
--payment-verification-key-file address/fees.vkey \
--stake-verification-key-file address/fees_stk.vkey \
--out-file address/fees.addr \
--$NETWORK_SELECTION 


# 3 signatures
$CARDANO_CLI address key-gen --verification-key-file address/key1.vkey --signing-key-file address/key1.skey --extended-key 
$CARDANO_CLI address key-gen --verification-key-file address/key2.vkey --signing-key-file address/key2.skey --extended-key 
$CARDANO_CLI address key-gen --verification-key-file address/key3.vkey --signing-key-file address/key3.skey --extended-key 
