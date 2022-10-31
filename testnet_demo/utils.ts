import * as fs from "fs";
import * as path from "path";
import execa from "execa";
import { collateralLovelace, tokenAPrefix, tokenBPrefix } from "./constants";
import { config } from "./config";

export type Amount = {
  unit: string;
  quantity: number;
};

export type Utxo = {
  txId: string;
  txIndex: number;
  amount: Amount[];
  datumHash?: string;
};

export const fromHex = (hex: string) => Buffer.from(hex, "hex");
export const toHex = (bytes: number[]) => Buffer.from(bytes).toString("hex");
export const asciiToHex = (str: string) =>
  Buffer.from(str, "ascii").toString("hex");
export const hexToAscii = (str: string) =>
  Buffer.from(str, "hex").toString("ascii");

export const isValidatorScriptInitialized = () => {
  return (
    fs.existsSync(`${config.OUT_DIR}/keys`) &&
    fs.existsSync(`${config.OUT_DIR}/validator.plutus`) &&
    fs.existsSync(`${config.OUT_DIR}/validator-hash.txt`) &&
    fs.existsSync(`${config.OUT_DIR}/minting.plutus`) &&
    fs.existsSync(`${config.OUT_DIR}/currency-id.txt`)
  );
};

export const initializeValidatorScript = async () => {
  const bootstrapId = getUnixTime();
  if (!fs.existsSync(`${config.OUT_DIR}/keys`))
    fs.mkdirSync(`${config.OUT_DIR}/keys`, { recursive: true });

  for (let x = 1; x <= config.MIN_SIG; x++) {
    await generateExtendedKeyPair(`signer_${x}`);
  }

  await execa("cabal", ["run", "script-dump"], { cwd: config.OUT_DIR });

  fs.writeFileSync(`${config.OUT_DIR}/bootstrap_id`, bootstrapId.toString());
};

export const sleep = (ms: number) => {
  return new Promise((resolve) => setTimeout(resolve, ms));
};

export const getBalanceValueFromCliUtxos = (utxos: Utxo[]) => {
  return utxos.reduce<any>((acc, utxo) => {
    utxo.amount.forEach((amount) => {
      acc[amount.unit] = (acc[amount.unit] || 0) + amount.quantity;
    });
    return acc;
  }, {});
};

export const stringFormatBalance = (balanceValue: any) => {
  return Object.keys(balanceValue).reduce((acc, key) => {
    const value = balanceValue[key];
    if (key === getCurrencyId() + getFractionTokenName()) {
      key = "Fractions";
    } else if (key !== "lovelace") {
      key = hexToAscii(key.substring(56));
    }
    return `${acc}${key}\t\t\t\t\t${value}\n`;
  }, "");
};

export const generateExtendedKeyPair = async (keyName: string) => {
  await execa("cardano-cli", [
    "address",
    "key-gen",
    "--verification-key-file",
    `${config.OUT_DIR}/keys/${keyName}.vkey`,
    "--signing-key-file",
    `${config.OUT_DIR}/keys/${keyName}.skey`,
    "--extended-key",
  ]);
};

export const isPaymentKeysExist = () => {
  const skey = path.join(config.OUT_DIR, "keys", "payment.skey");
  const vkey = path.join(config.OUT_DIR, "keys", "payment.vkey");
  const addr = path.join(config.OUT_DIR, "keys", "payment.addr");
  const changeSkey = path.join(config.OUT_DIR, "keys", "change.skey");
  const changeVkey = path.join(config.OUT_DIR, "keys", "change.vkey");
  const changeAddr = path.join(config.OUT_DIR, "keys", "change.addr");
  const policy = path.join(config.OUT_DIR, "keys", "payment.script");

  return (
    fs.existsSync(skey) &&
    fs.existsSync(vkey) &&
    fs.existsSync(addr) &&
    fs.existsSync(policy) &&
    fs.existsSync(changeSkey) &&
    fs.existsSync(changeVkey) &&
    fs.existsSync(changeAddr)
  );
};

export const networkFlagByNetworkId = () =>
  config.NETWORK_ID === 0 ? "--testnet-magic 2" : "--mainnet";

export const generateKeyPairWalletAddr = async (keyName: string) => {
  await execa("cardano-cli", [
    "address",
    "build",
    "--payment-verification-key-file",
    `${config.OUT_DIR}/keys/${keyName}.vkey`,
    "--out-file",
    `${config.OUT_DIR}/keys/${keyName}.addr`,
    ...networkFlagByNetworkId().split(" "),
  ]);
};

export const generateKeyHashByVKey = async (keyPath: string) => {
  return new Promise<string>(async (resolve, reject) => {
    try {
      const result = await execa("cardano-cli", [
        "address",
        "key-hash",
        "--payment-verification-key-file",
        keyPath,
      ]);
      resolve(result.stdout);
    } catch (ex) {
      reject(ex);
    }
  });
};

export const generatePaymentKeys = async () => {
  if (!fs.existsSync(`${config.OUT_DIR}/keys`))
    fs.mkdirSync(`${config.OUT_DIR}/keys`, { recursive: true });

  await generateExtendedKeyPair("payment");
  await generateKeyPairWalletAddr("payment");
  await generateExtendedKeyPair("change");
  await generateKeyPairWalletAddr("change");

  // Generate Policy Script for payment key
  const script = {
    keyHash: await generateKeyHashByVKey(`${config.OUT_DIR}/keys/payment.vkey`),
    type: "sig",
  };

  fs.writeFileSync(
    `${config.OUT_DIR}/keys/payment.script`,
    JSON.stringify(script)
  );
};

export const getUtxoByPaymentKey = async (keyName: string) => {
  // Query Raw UTXO from CLI
  const paymentAddr = getWalletAddress(keyName);
  const result = await execa("cardano-cli", [
    "query",
    "utxo",
    "--address",
    paymentAddr,
    ...networkFlagByNetworkId().split(" "),
  ]);

  // Prase Utxo into Utxo[] object
  return parseRawUtxo(result.stdout);
};

export const getValidatorUtxos = async (validatorAddr: string) => {
  const result = await execa("cardano-cli", [
    "query",
    "utxo",
    "--address",
    validatorAddr,
    ...networkFlagByNetworkId().split(" "),
  ]);
  return parseRawUtxo(result.stdout);
};

export const getTxUtxos = async (txHash: string) => {
  const result = await execa("cardano-cli", [
    "query",
    "utxo",
    "--tx-in",
    `${txHash}#0`,
    ...networkFlagByNetworkId().split(" "),
  ]);
  return parseRawUtxo(result.stdout);
};

export const parseRawUtxo = (rawUtxo: string) => {
  return rawUtxo
    .split("\n")
    .map((line, idx) => {
      if (idx > 1) {
        const lineSplit = line.split(" ");
        const amount = lineSplit.slice(13).join(" ").split("+");

        const utxo = {
          txId: lineSplit[0],
          txIndex: parseInt(lineSplit[5]),
          amount: amount
            .map((amount) => {
              const sanitizedAmountSplit = amount.trim().split(" ");
              if (!amount.includes("TxOutDatum")) {
                return {
                  unit: sanitizedAmountSplit[1].replace(".", ""),
                  quantity: parseInt(sanitizedAmountSplit[0]),
                } as Amount;
              }
            })
            .filter((x) => x) as Amount[],
        } as Utxo;

        const datumHashItem = amount.find((amount) =>
          amount.includes("TxOutDatumHash")
        );
        if (datumHashItem) {
          const datumHash = datumHashItem.split(" ")[3].slice(1, -1);
          utxo.datumHash = datumHash;
        }

        return utxo;
      }
    })
    .filter((x) => x) as Utxo[];
};

export const getValidatorAddress = async () => {
  const result = await execa("cardano-cli", [
    "address",
    "build",
    "--payment-script-file",
    `${config.OUT_DIR}/validator.plutus`,
    ...networkFlagByNetworkId().split(" "),
  ]);
  return result.stdout;
};

export const getWalletAddress = (keyName: string) => {
  return fs.readFileSync(`${config.OUT_DIR}/keys/${keyName}.addr`).toString();
};

export const getBootstrapId = () => {
  return fs.readFileSync(`${config.OUT_DIR}/bootstrap_id`).toString();
};

export const getCurrencyId = () => {
  return fs.readFileSync(`${config.OUT_DIR}/currency-id.txt`).toString();
};

export const getDatumHash = () => {
  return fs.readFileSync(`${config.OUT_DIR}/datum-hash.txt`).toString();
};

export const getTokenName = (prefix: string) => {
  const bootstrapId = getBootstrapId();
  return `${prefix}${bootstrapId}`;
};

export const getFractionTokenName = () => {
  try {
    return fs
      .readFileSync(`${config.OUT_DIR}/fraction-tokenname.txt`)
      .toString();
  } catch (err) {
    return "";
  }
};

export const getPolicyIdByScript = async (keyName: string) => {
  return new Promise<string>(async (resolve, reject) => {
    try {
      const result = await execa("cardano-cli", [
        "transaction",
        "policyid",
        "--script-file",
        `${config.OUT_DIR}/keys/${keyName}.script`,
      ]);
      resolve(result.stdout);
    } catch (ex) {
      reject(ex);
    }
  });
};

export const getUnixTime = () => {
  return Math.floor(Date.now() / 1000);
};

export const bootstrapWallet = async () => {
  try {
    const bufferLovelace = 5_000_000;
    const utxos = await getUtxoByPaymentKey("payment");
    const policyId = await getPolicyIdByScript("payment");
    const paymentWalletAddress = getWalletAddress("payment");
    const changeWalletAddress = getWalletAddress("change");
    const balance = getBalanceValueFromCliUtxos(utxos);
    const changeLovelace = 2_000_000 * (Object.keys(balance).length - 1);
    const mintLovelace = 4_000_000;
    const lovelaceToSend =
      balance["lovelace"] -
      changeLovelace -
      bufferLovelace -
      mintLovelace -
      collateralLovelace;

    const tokenA = `${policyId}.${asciiToHex(getTokenName(tokenAPrefix))}`;
    const tokenB = `${policyId}.${asciiToHex(getTokenName(tokenBPrefix))}`;

    let changeAssets = "";
    for (let k in balance) {
      if (k !== "lovelace") {
        const unit = k.substring(0, 56) + "." + k.substring(56);
        changeAssets += ` + ${balance[k]} ${unit}`;
      }
    }

    // prettier-ignore
    await execa("cardano-cli", [
      "transaction",
      "build",
      ...utxos.reduce((acc, utxo) => {
        return [...["--tx-in", `${utxo.txId}#${utxo.txIndex}`], ...acc];
      }, [] as string[]),
      "--tx-out", `${paymentWalletAddress} + ${lovelaceToSend}`,
      "--tx-out", `${paymentWalletAddress} + ${collateralLovelace}`,
      "--tx-out", `${paymentWalletAddress} + ${mintLovelace} + 1 ${tokenA} + 1 ${tokenB}`,
      ...(changeAssets.length > 0
        ? [
            "--tx-out", `${changeWalletAddress} + ${changeLovelace} + ${changeAssets.trim().substring(1).trim()}`,
          ]
        : []),
      "--change-address", paymentWalletAddress,
      "--mint", `1 ${tokenA} + 1 ${tokenB}`,
      "--mint-script-file", `${config.OUT_DIR}/keys/payment.script`,
      ...networkFlagByNetworkId().split(" "),
      "--out-file", `${config.OUT_DIR}/tx.raw`,
    ]);

    // prettier-ignore
    await execa("cardano-cli", [
      "transaction",
      "sign",
      ...networkFlagByNetworkId().split(" "),
      "--tx-body-file", `${config.OUT_DIR}/tx.raw`,
      "--signing-key-file", `${config.OUT_DIR}/keys/payment.skey`,
      "--out-file", `${config.OUT_DIR}/tx.signed`,
    ]);

    // prettier-ignore
    await execa("cardano-cli", [
      "transaction",
      "submit",
      ...networkFlagByNetworkId().split(" "),
      "--tx-file", `${config.OUT_DIR}/tx.signed`,
    ]);

    // prettier-ignore
    const result = await execa("cardano-cli", [
      "transaction",
      "txid",
      "--tx-file", `${config.OUT_DIR}/tx.signed`,
    ]);

    return result.stdout;
  } catch (ex) {
    console.log(ex);
  }
};

export const generateProtocolParams = async () => {
  try {
    const result = await execa("cardano-cli", [
      "query",
      "protocol-parameters",
      ...networkFlagByNetworkId().split(" "),
    ]);

    // Delete the file if it exists
    if (fs.existsSync(`${config.OUT_DIR}/protocol-params.json`))
      fs.unlinkSync(`${config.OUT_DIR}/protocol-params.json`);

    fs.writeFileSync(`${config.OUT_DIR}/protocol-params.json`, result.stdout);
  } catch (ex) {
    console.log(ex);
  }
};

export const calculateFractionTokenNameHash = async (utxo: Utxo) => {
  const valueToHash = [utxo.txIndex, ...fromHex(utxo.txId)];

  const { createHash } = require("crypto");
  const hash = await createHash("sha256")
    .update(toHex(valueToHash), "hex")
    .digest("hex");

  fs.writeFileSync(`${config.OUT_DIR}/fraction-tokenname.txt`, hash);

  return hash;
};

export const getCollateralUtxo = (utxos: Utxo[]) => {
  return utxos.find(
    (utxo) =>
      utxo.amount.length === 1 &&
      utxo.amount[0].quantity === collateralLovelace &&
      utxo.amount.find((a) => a.unit === "lovelace") !== undefined
  );
};

export const getFractionNftUtxo = (
  utxos: Utxo[],
  fractionTokenName: string
) => {
  return utxos.find((utxo) =>
    utxo.amount.find((a) => a.unit === fractionTokenName)
  );
};

export const getScriptUtxo = (utxos: Utxo[], datumHash: string) => {
  return utxos.find((utxo) => utxo.datumHash === datumHash);
};

export const getSecondTokenUtxo = (utxos: Utxo[]) => {
  return utxos.find(
    (utxo) =>
      utxo.amount.length === 2 &&
      utxo.amount.find(
        (a) =>
          a.unit.includes(asciiToHex(getTokenName(tokenBPrefix))) &&
          a.quantity === 1
      )
  );
};

export const waitForTxConf = async (txId: string) => {
  while (true) {
    const utxos = await getTxUtxos(txId);
    if (utxos.length > 0) {
      return;
    }

    await sleep(1000);
  }
};
