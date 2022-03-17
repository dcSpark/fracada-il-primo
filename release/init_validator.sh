. params.sh

echo "Backup old scripts"
[ -f "plutus/validator.plutus" ] && mv "plutus/validator.plutus" plutus/validator.plutus.bk
[ -f "plutus/validator-hash.txt" ] && mv "plutus/validator-hash.txt" plutus/validator-hash.txt.bk
[ -f "plutus/minting.plutus" ] && mv "plutus/minting.plutus" plutus/minting.plutus.bk
[ -f "plutus/currency-id.txt" ] && mv "plutus/currency-id.txt" plutus/currency-id.txt.bk

#generate script
. script-dump.sh ${NFT_CURRENCY} ${NFT_TOKEN} ${FRACT_TOKEN} ${MIN_SIGS} < authorized_keys.txt

#Build validator's address
$CARDANO_CLI address build --payment-script-file validator.plutus --$NETWORK_SELECTION --out-file wallets/validator.addr

#Query protocol parameters
$CARDANO_CLI query protocol-parameters --$NETWORK_SELECTION --out-file pparams.json

#MOVE SCRIPT TO PLUTUS FOLDER
mv validator.plutus plutus/
mv validator-hash.txt plutus/
mv minting.plutus plutus/
mv currency-id.txt plutus/

#declare FRACT_CURRENCY incase its the first time generatings
export FRACT_CURRENCY=$(cat ./plutus/currency-id.txt)
