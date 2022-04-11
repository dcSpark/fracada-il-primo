import * as fs from 'fs';
import * as path from 'path';
import fetch from 'node-fetch';
import execa from 'execa';
import { BlockFrostAPI } from '@blockfrost/blockfrost-js';
import {
  Address,
  AssetName,
  Assets,
  BaseAddress,
  BigNum,
  Bip32PrivateKey,
  Bip32PublicKey,
  CoinSelectionStrategyCIP2,
  Ed25519KeyHash,
  LinearFee,
  MultiAsset,
  NativeScript,
  ScriptHash,
  ScriptHashNamespace,
  ScriptPubkey,
  StakeCredential,
  TransactionBuilder,
  TransactionBuilderConfig,
  TransactionBuilderConfigBuilder,
  TransactionHash,
  TransactionInput,
  TransactionInputs,
  TransactionOutput,
  TransactionOutputs,
  TransactionUnspentOutput,
  TransactionUnspentOutputs,
  Value
} from '@emurgo/cardano-serialization-lib-nodejs';
import { mnemonicToEntropy } from 'bip39';
import * as rm from 'rimraf';

export type AccountPubKeyPair = {
  paymentKey: Bip32PublicKey,
  stakeKey: Bip32PublicKey
}

export type Amount = {
  unit: string,
  quantity: number
}

export type Utxo = {
  txId: string,
  txIndex: number,
  amount: Amount[]
}

function harden(num: number): number {
  return 0x80000000 + num;
}

export const fromHex = (hex: string) => Buffer.from(hex, "hex");
export const toHex = (bytes: Uint8Array) => Buffer.from(bytes).toString("hex");
export const asciiToHex = (str: string) => Buffer.from(str, "ascii").toString("hex");
export const hexToAscii = (str: string) => Buffer.from(str, "hex").toString("ascii");

export const getAccountKeyByMnemonic = (mnemonic: string, index: number = 0) => {
  const entropy = mnemonicToEntropy(mnemonic);
  const rootKey = Bip32PrivateKey.from_bip39_entropy(
    Buffer.from(entropy, 'hex'),
    Buffer.from('')
  );

  const accountKey = rootKey
    .derive(harden(1852))
    .derive(harden(1815))
    .derive(harden(index));

  return accountKey;
}

export const getPubKeyPairByAccountKey = (accountKey: Bip32PrivateKey, index: number = 0) => {
  const paymentKey = accountKey
    .derive(0) // external
    .derive(index)
    .to_public();

  const stakeKey = accountKey
    .derive(2) // chimeric
    .derive(index)
    .to_public();

  return {
    paymentKey,
    stakeKey
  } as AccountPubKeyPair;
}

export const getWalletAddressByKeyPair = (keyPair: AccountPubKeyPair, networkId: number) => {
  const baseAddr = BaseAddress.new(
    networkId,
    StakeCredential.from_keyhash(keyPair.paymentKey.to_raw_key().hash()),
    StakeCredential.from_keyhash(keyPair.stakeKey.to_raw_key().hash()),
  );
  return baseAddr.to_address().to_bech32();
}

export const getUtxosByWalletAddress = async (api: BlockFrostAPI, walletAddress: string) => {
  try {
    const utxo = await api.addressesUtxosAll(walletAddress);
    return utxo;
  } catch (ex) {
    return [];
  }
}

export const getProtocolParams = async (api: BlockFrostAPI) => {
  const latestEpoch = await api.epochsLatest();
  return await api.epochsParameters(latestEpoch.epoch);
};

export const getPolicyIdByAddress = (address: string) => {
  const addr = Address.from_bech32(address) as Address;
  const baseAddr = BaseAddress.from_address(addr) as BaseAddress;
  const pKeyHash = baseAddr
    .payment_cred()
    .to_keyhash() as Ed25519KeyHash;

  const script = ScriptPubkey.new(pKeyHash);
  const nativeScript = NativeScript.new_script_pubkey(script);
  const scriptHash = ScriptHash.from_bytes(
    nativeScript.hash(ScriptHashNamespace.NativeScript).to_bytes()
  );
  const policyId = toHex(scriptHash.to_bytes());
  return policyId;
}

export const resetTest = async (outDir: string) => {
  if (fs.existsSync(`${outDir}/keys`)) rm.sync(`${outDir}/keys`);
}

export const isValidatorScriptInitialized = (outDir: string) => {
  return fs.existsSync(`${outDir}/keys`) &&
    fs.existsSync(`${outDir}/validator.plutus`) &&
    fs.existsSync(`${outDir}/validator-hash.txt`) &&
    fs.existsSync(`${outDir}/minting.plutus`) &&
    fs.existsSync(`${outDir}/currency-id.txt`);
}

export const initializeValidatorScript = async (policyId: string, outDir: string, minSig: number) => {

  const bootstrapId = getUnixTime();
  const fakeNftName = `FakeNft_A_${bootstrapId}`;
  if (!fs.existsSync(`${outDir}/keys`)) fs.mkdirSync(`${outDir}/keys`, { recursive: true });

  for (let x = 1; x <= minSig; x++) {
    await generateExtendedKeyPair(outDir, `signer_${x}`);
  }

  const scriptDump = execa('cabal',
    ['run', 'script-dump', '--', policyId, fakeNftName, 'NftFractions', `${minSig}`],
    { cwd: outDir }
  );

  scriptDump.stdin?.write("keys/signer_1.vkey\nkeys/signer_2.vkey\nkeys/signer_3.vkey");
  scriptDump.stdin?.end();
  await scriptDump;

  fs.writeFileSync(`${outDir}/bootstrap_id`, bootstrapId.toString());
}

export const sleep = (ms: number) => {
  return new Promise(resolve => setTimeout(resolve, ms));
}

export const getBalanceValueFromBfUtxos = (utxos: {
  tx_hash: string;
  tx_index: number;
  output_index: number;
  amount: {
    unit: string;
    quantity: string;
  }[];
  block: string;
  data_hash: string | null;
}[]) => {
  return utxos.reduce<any>((acc, utxo) => {
    utxo.amount.forEach(amount => {
      // @TODO: should use BigNum here
      acc[amount.unit] = (acc[amount.unit] || 0) + parseInt(amount.quantity);
    });
    return acc;
  }, {});
}

export const getBalanceValueFromCliUtxos = (utxos: Utxo[]) => {
  return utxos.reduce<any>((acc, utxo) => {
    utxo.amount.forEach(amount => {
      acc[amount.unit] = (acc[amount.unit] || 0) + amount.quantity;
    });
    return acc;
  }, {});
}

export const stringFormatBalance = (balanceValue: any) => {
  return Object.keys(balanceValue).reduce((acc, key) => {
    const value = balanceValue[key];
    if (key !== 'lovelace') key = hexToAscii(key.substring(56));
    return `${acc}${key}\t\t\t\t\t${value}\n`;
  }, '');
}

export const AssetValue = (lovelace: BigNum, policyIdHex: string, assetNameHex: string, amount: BigNum) => {
  const value = Value.new(lovelace);
  const ma = MultiAsset.new();
  const assets = Assets.new();

  assets.insert(
    AssetName.new(fromHex(assetNameHex)),
    amount
  );

  ma.insert(
    ScriptHash.from_bytes(fromHex(policyIdHex)),
    assets
  );

  value.set_multiasset(ma)
  return value;
}

export const txBuilderFromProtocolParams = (protocolParams: any) => {
  const txBuilderConfBuilder = TransactionBuilderConfigBuilder.new()
    .coins_per_utxo_word(
      BigNum.from_str(protocolParams.coins_per_utxo_word ?? '0')
    )
    .fee_algo(
      LinearFee.new(
        BigNum.from_str(protocolParams.min_fee_a.toString()),
        BigNum.from_str(protocolParams.min_fee_b.toString())
      )
    )
    .key_deposit(
      BigNum.from_str(protocolParams.key_deposit)
    )
    .max_tx_size(protocolParams.max_tx_size).max_value_size(
      parseInt(protocolParams.max_val_size ?? '5000')
    )
    .pool_deposit(
      BigNum.from_str(protocolParams.pool_deposit)
    );

  return TransactionBuilder.new(txBuilderConfBuilder.build());
}

export const bootstrap = async (api: BlockFrostAPI, accountKey: Bip32PrivateKey, utxos: {
  tx_hash: string;
  tx_index: number;
  output_index: number;
  amount: {
    unit: string;
    quantity: string;
  }[];
  block: string;
  data_hash: string | null;
}[]) => {
  const inputs = TransactionUnspentOutputs.new();
  let bootstrapValue = Value.new(BigNum.from_str('0'));
  let changeValue = Value.new(BigNum.from_str('0'));
  const walletAddress = getWalletAddressByKeyPair(getPubKeyPairByAccountKey(accountKey), 0);
  const changeWalletAddress = getWalletAddressByKeyPair(getPubKeyPairByAccountKey(accountKey), 1);
  const policyId = getPolicyIdByAddress(walletAddress);

  utxos.forEach(utxo => {
    const txInput = TransactionInput.new(
      TransactionHash.from_bytes(fromHex(utxo.tx_hash)),
      utxo.tx_index,
    );

    let inputValue = Value.new(BigNum.from_str('0'));

    utxo.amount.forEach(a => {
      if (a.unit === 'lovelace') { // keep lovelace
        const amountValue = Value.new(
          BigNum.from_str(a.quantity)
        );
        inputValue = inputValue.checked_add(amountValue);
        bootstrapValue = bootstrapValue.checked_add(amountValue);

      } else if (a.unit.includes(policyId)) { // return self minted NFTs
        const amountValue = AssetValue(
          BigNum.from_str('0'),
          policyId,
          a.unit.substring(56),
          BigNum.from_str(a.quantity)
        );
        inputValue = inputValue.checked_add(amountValue);
        bootstrapValue = bootstrapValue.checked_add(amountValue);
      }
      else { // separate random NFTs to be sent change account
        const amountValue = AssetValue(
          BigNum.from_str('0'),
          a.unit.substring(0, 56),
          a.unit.substring(56),
          BigNum.from_str(a.quantity)
        );
        inputValue = inputValue.checked_add(amountValue);
        changeValue = changeValue.checked_add(amountValue);
      }
    });

    inputs.add(
      TransactionUnspentOutput.new(
        txInput,
        TransactionOutput.new(
          Address.from_bech32(walletAddress),
          inputValue
        )
      )
    );
  });



  // Build Transaction
  const protocolParams = await getProtocolParams(api);
  const txBuiler = txBuilderFromProtocolParams(protocolParams);

  // Generate change utxo
  const changeMa = changeValue.multiasset();
  if (changeMa !== undefined && changeMa.len() > 0) {
    const changeLovelaceAdd = Value.new(
      BigNum.from_str((2_000_000 * changeMa.len()).toString())
    );

    changeValue = changeValue.checked_add(changeLovelaceAdd);
    bootstrapValue = bootstrapValue.checked_sub(changeLovelaceAdd);

    txBuiler.add_output(
      TransactionOutput.new(
        Address.from_bech32(changeWalletAddress),
        changeValue
      ));
  }
  // Generate bootstrap utxos

  // Prepare collateral and spare ADA for fees
  bootstrapValue = bootstrapValue.checked_sub(
    Value.new(BigNum.from_str('10000000'))
  );
  txBuiler.add_output(
    TransactionOutput.new(
      Address.from_bech32(walletAddress),
      bootstrapValue
    )
  );

  // Collateral Output
  txBuiler.add_output(
    TransactionOutput.new(
      Address.from_bech32(walletAddress),
      Value.new(BigNum.from_str('5000000'))
    )
  );

  txBuiler.add_inputs_from(
    inputs,
    CoinSelectionStrategyCIP2.RandomImproveMultiAsset
  );
}

export const generateExtendedKeyPair = async (outDir: string, keyName: string) => {
  await execa('cardano-cli', [
    'address',
    'key-gen',
    '--verification-key-file',
    `${outDir}/keys/${keyName}.vkey`,
    '--signing-key-file',
    `${outDir}/keys/${keyName}.skey`,
    '--extended-key']);
}

export const isPaymentKeysExist = (outDir: string) => {
  const skey = path.join(outDir, 'keys', 'payment.skey');
  const vkey = path.join(outDir, 'keys', 'payment.vkey');
  const addr = path.join(outDir, 'keys', 'payment.addr');
  const changeSkey = path.join(outDir, 'keys', 'change.skey');
  const changeVkey = path.join(outDir, 'keys', 'change.vkey');
  const changeAddr = path.join(outDir, 'keys', 'change.addr');
  const policy = path.join(outDir, 'keys', 'payment.script');

  return fs.existsSync(skey) &&
    fs.existsSync(vkey) &&
    fs.existsSync(addr) &&
    fs.existsSync(policy) &&
    fs.existsSync(changeSkey) &&
    fs.existsSync(changeVkey) &&
    fs.existsSync(changeAddr);
}

export const networkFlagByNetworkId = (networkId: number) => networkId === 0 ? '--testnet-magic 1097911063' : '--mainnet';

export const generateKeyPairWalletAddr = async (outDir: string, keyName: string, networkId: number) => {
  await execa('cardano-cli', [
    'address',
    'build',
    '--payment-verification-key-file',
    `${outDir}/keys/${keyName}.vkey`,
    '--out-file',
    `${outDir}/keys/${keyName}.addr`,
    ...networkFlagByNetworkId(networkId).split(' ')
  ]);
}

export const generateKeyHashByVKey = async (keyPath: string) => {
  return new Promise<string>(async (resolve, reject) => {
    try {
      const result = await execa('cardano-cli', [
        'address',
        'key-hash',
        '--payment-verification-key-file',
        keyPath
      ]);
      resolve(result.stdout);
    } catch (ex) {
      reject(ex);
    }
  });
}

export const generatePaymentKeys = async (outDir: string, networkId: number) => {
  if (!fs.existsSync(`${outDir}/keys`)) fs.mkdirSync(`${outDir}/keys`, { recursive: true });

  await generateExtendedKeyPair(outDir, 'payment');
  await generateKeyPairWalletAddr(outDir, 'payment', networkId);
  await generateExtendedKeyPair(outDir, 'change');
  await generateKeyPairWalletAddr(outDir, 'change', networkId);

  // Generate Policy Script for payment key
  const script = {
    keyHash: await generateKeyHashByVKey(`${outDir}/keys/payment.vkey`),
    type: 'sig'
  };

  fs.writeFileSync(`${outDir}/keys/payment.script`, JSON.stringify(script));
}

export const getUtxoByPaymentKey = async (outDir: string, keyName: string, networkId: number) => {
  // Query Raw UTXO from CLI
  const paymentAddr = await getWalletAddress(outDir, keyName);
  const result = await execa('cardano-cli', [
    'query',
    'utxo',
    '--address',
    paymentAddr,
    ...networkFlagByNetworkId(networkId).split(' ')
  ]);

  // Prase Utxo into Utxo[] object
  return parseRawUtxo(result.stdout);
}

export const getValidatorUtxos = async (validatorAddr: string, networkId: number) => {
  const result = await execa('cardano-cli', [
    'query',
    'utxo',
    '--address',
    validatorAddr,
    ...networkFlagByNetworkId(networkId).split(' ')
  ]);
  return parseRawUtxo(result.stdout);
}

export const parseRawUtxo = (rawUtxo: string) => {
  return rawUtxo.split('\n').map((line, idx) => {
    if (idx > 1) {
      const lineSplit = line.split(' ');
      const amountString = lineSplit.slice(13).join(' ');
      return {
        txId: lineSplit[0],
        txIndex: parseInt(lineSplit[5]),
        amount: amountString.split('+').map((amount) => {
          const sanitizedAmountSplit = amount.trim().split(' ');
          if (!amount.includes('TxOutDatum')) {
            return {
              unit: sanitizedAmountSplit[1].replace('.', ''),
              quantity: parseInt(sanitizedAmountSplit[0])
            } as Amount;
          }
        }).filter(x => x) as Amount[]
      } as Utxo;
    }
  }).filter(x => x) as Utxo[]
}

export const getValidatorAddress = async (outDir: string, networkId: number) => {
  const result = await execa('cardano-cli', [
    'address',
    'build',
    '--payment-script-file',
    `${outDir}/validator.plutus`,
    ...networkFlagByNetworkId(networkId).split(' ')
  ]);
  return result.stdout;
}

export const getWalletAddress = async (outDir: string, keyName: string) => {
  return fs.readFileSync(`${outDir}/keys/${keyName}.addr`).toString();
}

export const getPolicyIdByScript = async (outDir: string, keyName: string) => {
  return new Promise<string>(async (resolve, reject) => {
    try {
      const result = await execa('cardano-cli', [
        'transaction',
        'policyid',
        '--script-file',
        `${outDir}/keys/${keyName}.script`
      ]);
      resolve(result.stdout);
    } catch (ex) {
      reject(ex);
    }
  });
}

export const getUnixTime = () => {
  return Math.floor(+new Date() / 1000)
}

export const bootstrapWallet = async (outDir: string, networkId: number) => {
  try {
    const bufferLovelace = 5_000_000;
    const utxos = await getUtxoByPaymentKey(outDir, 'payment', networkId);
    const policyId = await getPolicyIdByScript(outDir, 'payment');
    const paymentWalletAddress = await getWalletAddress(outDir, 'payment');
    const changeWalletAddress = await getWalletAddress(outDir, 'change');
    const balance = getBalanceValueFromCliUtxos(utxos);
    const changeLovelace = 2_000_000 * (Object.keys(balance).length - 1);
    const mintLovelace = 4_000_000;
    const collateralLovelace = 5_000_000;
    const lovelaceToSend = balance['lovelace'] - changeLovelace - bufferLovelace - mintLovelace - collateralLovelace;
    const bootstrapId = fs.readFileSync(`${outDir}/bootstrap_id`).toString();
    const fakeNftA = `${policyId}.${asciiToHex(`FakeNft_A_${bootstrapId}`)}`;
    const fakeNftB = `${policyId}.${asciiToHex(`FakeNft_B_${bootstrapId}`)}`;
    let changeAssets = '';

    for (let k in balance) {
      if (k !== 'lovelace') {
        const unit = k.substring(0, 56) + '.' + k.substring(56);
        changeAssets += ` + ${balance[k]} ${unit}`;
      }
    }

    await execa('cardano-cli', [
      'transaction', 'build',
      ...utxos.reduce((acc, utxo) => {
        return [...['--tx-in', `${utxo.txId}#${utxo.txIndex}`], ...acc]
      }, [] as string[]),
      '--tx-out', `${paymentWalletAddress}+${lovelaceToSend}`,
      '--tx-out', `${paymentWalletAddress}+${collateralLovelace}`,
      '--tx-out', `${paymentWalletAddress}+${mintLovelace}+1 ${fakeNftA} + 1 ${fakeNftB}`,
      ...(changeAssets.length > 0 ? (['--tx-out', `${changeWalletAddress}+${changeLovelace}+${changeAssets.trim().substring(1).trim()}`]) : []),
      '--change-address', paymentWalletAddress,
      '--mint', `1 ${fakeNftA} + 1 ${fakeNftB}`,
      '--mint-script-file', `${outDir}/keys/payment.script`,
      ...networkFlagByNetworkId(networkId).split(' '),
      '--out-file', `${outDir}/tx.raw`
    ]);

    await execa('cardano-cli', [
      'transaction', 'sign',
      ...networkFlagByNetworkId(networkId).split(' '),
      '--tx-body-file', `${outDir}/tx.raw`,
      '--signing-key-file', `${outDir}/keys/payment.skey`,
      '--out-file', `${outDir}/tx.signed`
    ]);

    await execa('cardano-cli', [
      'transaction', 'submit',
      ...networkFlagByNetworkId(networkId).split(' '),
      '--tx-file', `${outDir}/tx.signed`,
    ]);

    const result = await execa('cardano-cli', [
      'transaction', 'txid',
      '--tx-file', `${outDir}/tx.signed`
    ]);

    return result.stdout;

  } catch (ex) {
    console.log(ex);
  }
}


export const generateProtocolParams = async (outDir: string, networkId: number) => {
  try {
    const result = await execa('cardano-cli', [
      'query',
      'protocol-parameters',
      ...networkFlagByNetworkId(networkId).split(' ')
    ]);

    // Delete the file if it exists
    if (fs.existsSync(`${outDir}/protocol-params.json`))
      fs.unlinkSync(`${outDir}/protocol-params.json`);

    fs.writeFileSync(`${outDir}/protocol-params.json`, result.stdout);

  } catch (ex) {
    console.log(ex);
  }
}

export const lockNft = async (outDir: string, networkId: number) => {

  const bufferLovelace = 10_000_000;
  const validatorLovelace = 2_000_000;
  const mintLovelace = 2_000_000;

  const utxos = await getUtxoByPaymentKey(outDir, 'payment', networkId);
  const collateralUtxo = getCollateralUtxo(utxos);
  const policyId = await getPolicyIdByScript(outDir, 'payment');
  const paymentWalletAddress = await getWalletAddress(outDir, 'payment');
  const validatorAddr = await getValidatorAddress(outDir, networkId);
  const bootstrapId = fs.readFileSync(`${outDir}/bootstrap_id`).toString();
  const balance = await getBalanceValueFromCliUtxos(utxos);
  const changeLovelace = balance['lovelace'] - validatorLovelace - bufferLovelace - mintLovelace;
  const fractionCurrencyId = fs.readFileSync(`${outDir}/currency-id.txt`).toString();
  const fractions = 100;
  let changeAssets = '';

  for (let k in balance) {
    if (k !== 'lovelace' && k !== `${policyId}${asciiToHex(`FakeNft_A_${bootstrapId}`)}`) {
      const unit = k.substring(0, 56) + '.' + k.substring(56);
      changeAssets += ` + ${balance[k]} ${unit}`;
    }
  }

  if (collateralUtxo !== undefined) {
    // refresh protocol params
    await generateProtocolParams(outDir, networkId);

    // build lock tx datum
    await execa('cabal', [
      'run', 'build-datum ', '--', 'new',
      fractionCurrencyId, `NftFractions`, `${fractions}`,
      policyId, `FakeNft_A_${bootstrapId}`
    ], { cwd: outDir });

    // build tx
    await execa('cardano-cli', [
      'transaction', 'build',
      ...utxos.reduce((acc, utxo) => {
        if (`${utxo.txId}#${utxo.txIndex}` !== `${collateralUtxo.txId}#${collateralUtxo.txIndex}`) {
          return [...['--tx-in', `${utxo.txId}#${utxo.txIndex}`], ...acc];
        }
        else
          return acc;
      }, [] as string[]),
      '--tx-in-collateral', `${collateralUtxo.txId}#${collateralUtxo.txIndex}`,
      '--tx-out', `${validatorAddr}+${validatorLovelace}+1 ${policyId}.${asciiToHex(`FakeNft_A_${bootstrapId}`)}`,
      '--tx-out-datum-embed-file', `${outDir}/datum.json`,
      ...(changeAssets.length > 0 ? (['--tx-out', `${paymentWalletAddress}+${changeLovelace}+${changeAssets.trim().substring(1).trim()}`]) : ['--tx-out', `${paymentWalletAddress}+${changeLovelace}`]),
      '--tx-out', `${paymentWalletAddress}+${mintLovelace}+${fractions} ${fractionCurrencyId}.${asciiToHex(`NftFractions`)}`,
      '--mint', `${fractions} ${fractionCurrencyId}.${asciiToHex(`NftFractions`)}`,
      '--mint-script-file', `${outDir}/minting.plutus`,
      '--mint-redeemer-value', '{}',
      '--change-address', paymentWalletAddress,
      '--protocol-params-file', `${outDir}/protocol-params.json`,
      ...networkFlagByNetworkId(networkId).split(' '),
      '--out-file', `${outDir}/tx.raw`
    ]);

    await execa('cardano-cli', [
      'transaction', 'sign',
      ...networkFlagByNetworkId(networkId).split(' '),
      '--tx-body-file', `${outDir}/tx.raw`,
      '--signing-key-file', `${outDir}/keys/payment.skey`,
      '--out-file', `${outDir}/tx.signed`
    ]);

    await execa('cardano-cli', [
      'transaction', 'submit',
      ...networkFlagByNetworkId(networkId).split(' '),
      '--tx-file', `${outDir}/tx.signed`,
    ]);

    const result = await execa('cardano-cli', [
      'transaction', 'txid',
      '--tx-file', `${outDir}/tx.signed`
    ]);

    return result.stdout;
  }
}


export const unlockNft = async (outDir: string, networkId: number) => {
  const paymentAddr = await getWalletAddress(outDir, 'payment');
  const validatorAddr = await getValidatorAddress(outDir, networkId);
  const validatorUtxos = await getValidatorUtxos(validatorAddr, networkId);
  const paymentUtxos = await getUtxoByPaymentKey(outDir, 'payment', networkId);
  const collateralUtxo = getCollateralUtxo(paymentUtxos) as Utxo;
  const fractionUtxo = getFractionNftUtxo(paymentUtxos) as Utxo;
  const nftUnit = validatorUtxos[0].amount.find(a => a.unit !== 'lovelace')?.unit;
  const fractionAmount = fractionUtxo.amount.find(a => a.unit !== 'lovelace') as Amount;

  // build tx
  await execa('cardano-cli', [
    'transaction', 'build',
    '--tx-in', `${fractionUtxo.txId}#${fractionUtxo.txIndex}`,
    '--tx-in', `${validatorUtxos[0].txId}#${validatorUtxos[0].txIndex}`,
    '--tx-in-script-file', `${outDir}/validator.plutus`,
    '--tx-in-datum-file', `${outDir}/datum.json`,
    '--tx-in-redeemer-file', `empty-redeemer.json`,
    '--tx-in-collateral', `${collateralUtxo.txId}#${collateralUtxo.txIndex}`,
    '--tx-out', `${paymentAddr}+2000000+1 ${nftUnit?.substring(0, 56)}.${nftUnit?.substring(56)}`,
    '--mint', `-${fractionAmount.quantity} ${fractionAmount.unit.substring(0, 56)}.${fractionAmount.unit.substring(56)}`,
    '--mint-script-file', `${outDir}/minting.plutus`,
    '--mint-redeemer-value', '{}',
    '--change-address', paymentAddr,
    '--protocol-params-file', `${outDir}/protocol-params.json`,
    ...networkFlagByNetworkId(networkId).split(' '),
    '--out-file', `${outDir}/tx.raw`
  ]);

  await execa('cardano-cli', [
    'transaction', 'sign',
    ...networkFlagByNetworkId(networkId).split(' '),
    '--tx-body-file', `${outDir}/tx.raw`,
    '--signing-key-file', `${outDir}/keys/payment.skey`,
    '--out-file', `${outDir}/tx.signed`
  ]);

  await execa('cardano-cli', [
    'transaction', 'submit',
    ...networkFlagByNetworkId(networkId).split(' '),
    '--tx-file', `${outDir}/tx.signed`,
  ]);

  const result = await execa('cardano-cli', [
    'transaction', 'txid',
    '--tx-file', `${outDir}/tx.signed`
  ]);

  return result.stdout;
}

export const getCollateralUtxo = (utxos: Utxo[]) => {
  return utxos.find(
    utxo => utxo.amount.length === 1 &&
      (utxo.amount[0].quantity === 5_000_000) &&
      utxo.amount.find(a => a.unit === 'lovelace') !== undefined);
}

export const getFractionNftUtxo = (utxos: Utxo[]) => {
  return utxos.find(
    utxo => utxo.amount.find(a => a.unit.includes('4e66744672616374696f6e73')) !== undefined
  );
}

export const waitForTxConf = async (txId: string, networkId: number) => {
  while (true) {
    const networkName = networkId === 1 ? 'mainnet' : 'testnet';
    const req = await fetch(`https://${networkName}-cardano.flint-wallet.com/v2/txs/get`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json'
      },
      body: JSON.stringify({
        txHashes: [txId]
      })
    });
    const result = await req.json();
    if (result[txId] !== undefined) break;
    await sleep(10_000); // wait for half block time
  }
}