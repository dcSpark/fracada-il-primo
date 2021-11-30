. exec_paths.sh

echo "sign datum"
${SIGN} datum-hash.txt address/key1.skey d1.sign
${SIGN} datum-hash.txt address/key2.skey d2.sign
${SIGN} datum-hash.txt address/key3.skey d3.sign

echo d1.sign > signaturefiles.txt
echo d2.sign >> signaturefiles.txt
echo d3.sign >> signaturefiles.txt