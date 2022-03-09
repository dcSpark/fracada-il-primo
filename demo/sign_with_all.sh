
echo "sign datum"
sign datum-hash.txt address/key1.skey d1.sign
sign datum-hash.txt address/key2.skey d2.sign
sign datum-hash.txt address/key3.skey d3.sign

echo d1.sign > signaturefiles.txt
echo d2.sign >> signaturefiles.txt
echo d3.sign >> signaturefiles.txt