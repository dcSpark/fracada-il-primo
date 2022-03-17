. ./demo_config.sh

echo "sign datum"
. sign.sh datum-hash.txt wallets/key1.skey d1.sign
. sign.sh datum-hash.txt wallets/key2.skey d2.sign
. sign.sh datum-hash.txt wallets/key3.skey d3.sign

echo d1.sign > signaturefiles.txt
echo d2.sign >> signaturefiles.txt
echo d3.sign >> signaturefiles.txt