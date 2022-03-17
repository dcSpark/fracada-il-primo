export PATH=../release:../release/bin:$PATH

export NFT_CURRENCY=$(cardano-cli transaction policyid --script-file wallets/policy.script)
export NFT_TOKEN="FakeNFT_A"
export FRACT_TOKEN="FracadaTokenA"
export MIN_SIGS=2