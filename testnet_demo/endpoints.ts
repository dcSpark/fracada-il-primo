import execa from "execa";
import { config } from "./config";
import {
  validatorLovelace,
  tokenAPrefix,
  validityToken,
  tokenBPrefix,
  mintLovelace,
} from "./constants";
import {
  Amount,
  asciiToHex,
  calculateFractionTokenNameHash,
  generateKeyHashByVKey,
  generateProtocolParams,
  getBalanceValueFromCliUtxos,
  getCollateralUtxo,
  getCurrencyId,
  getDatumHash,
  getFractionNftUtxo,
  getFractionTokenName,
  getPolicyIdByScript,
  getScriptUtxo,
  getSecondTokenUtxo,
  getTokenName,
  getUtxoByPaymentKey,
  getValidatorAddress,
  getValidatorUtxos,
  getWalletAddress,
  networkFlagByNetworkId,
  Utxo,
} from "./utils";

export const lockNft = async () => {
  const bufferLovelace = 10_000_000;

  const utxos = await getUtxoByPaymentKey("payment");
  const collateralUtxo = getCollateralUtxo(utxos);
  const policyId = await getPolicyIdByScript("payment");
  const paymentWalletAddress = getWalletAddress("payment");
  const validatorAddr = await getValidatorAddress();
  const balance = await getBalanceValueFromCliUtxos(utxos);
  const changeLovelace =
    balance["lovelace"] - validatorLovelace - bufferLovelace - mintLovelace;
  const fractionCurrencyId = getCurrencyId();
  const fractions = 100;

  const utxo = utxos[utxos.length - 1];
  const fractionTokenName = await calculateFractionTokenNameHash(utxo);

  const lockedTokenName = asciiToHex(getTokenName(tokenAPrefix));
  let changeAssets = "";
  for (let k in balance) {
    if (k !== "lovelace" && k !== `${policyId}${lockedTokenName}`) {
      const unit = k.substring(0, 56) + "." + k.substring(56);
      changeAssets += ` + ${balance[k]} ${unit}`;
    }
  }

  if (!collateralUtxo) {
    return undefined;
  }

  await generateProtocolParams();

  // prettier-ignore
  const args = [
    "run", "build-datum ", "--",
    "new", fractionCurrencyId, `${utxo.txId}`, `${utxo.txIndex}`, `${fractions}`, `${config.MIN_SIG}`,
  ];

  // add pubkeyhashes of signers to args
  for (let x = 1; x <= config.MIN_SIG; x++) {
    args.push(
      await generateKeyHashByVKey(`${config.OUT_DIR}/keys/signer_${x}.vkey`)
    );
  }

  // build lock tx datum
  await execa("cabal", args, { cwd: config.OUT_DIR });

  // prettier-ignore
  await execa(
    "cabal", ["run", "build-redeemer", "--", "InitialMint", `${utxo.txId}`, `${utxo.txIndex}`],
    { cwd: config.OUT_DIR }
  );

  const lockedToken = `1 ${policyId}.${lockedTokenName}`;
  const fractionTokenUnit = `${fractions} ${fractionCurrencyId}.${fractionTokenName}`;
  const validityTokenUnit = `1 ${fractionCurrencyId}.${asciiToHex(
    validityToken
  )}`;

  // prettier-ignore
  await execa("cardano-cli", [
    "transaction",
    "build",
    ...utxos.reduce((acc, utxo) => {
      if ( `${utxo.txId}#${utxo.txIndex}` !== `${collateralUtxo.txId}#${collateralUtxo.txIndex}`) {
        return [...["--tx-in", `${utxo.txId}#${utxo.txIndex}`], ...acc];
      } else return acc;
    }, [] as string[]),
    "--tx-in-collateral", `${collateralUtxo.txId}#${collateralUtxo.txIndex}`,
    "--tx-out", `${validatorAddr} + ${validatorLovelace} + ${lockedToken} + ${validityTokenUnit}`,
    "--tx-out-datum-embed-file", `${config.OUT_DIR}/datum.json`,
    ...(changeAssets.length > 0
      ? [
          "--tx-out", `${paymentWalletAddress} + ${changeLovelace}${changeAssets}`,
        ]
      : ["--tx-out", `${paymentWalletAddress} + ${changeLovelace}`]),
    "--tx-out", `${paymentWalletAddress} + ${mintLovelace} + ${fractionTokenUnit}`,
    "--mint", `${fractionTokenUnit} + ${validityTokenUnit}`,
    "--mint-script-file", `${config.OUT_DIR}/minting.plutus`,
    "--mint-redeemer-file", `${config.OUT_DIR}/redeemer.json`,
    "--change-address", paymentWalletAddress,
    "--protocol-params-file", `${config.OUT_DIR}/protocol-params.json`,
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
};

export const unlockNft = async () => {
  const fractionCurrencyId = getCurrencyId();
  const fractionTokenName = getFractionTokenName();
  const fractionToken = `${fractionCurrencyId}${fractionTokenName}`;

  const datumHash = getDatumHash();
  const paymentAddr = getWalletAddress("payment");
  const validatorAddr = await getValidatorAddress();

  const validatorUtxos = await getValidatorUtxos(validatorAddr);
  const paymentUtxos = await getUtxoByPaymentKey("payment");
  const scriptUtxo = getScriptUtxo(validatorUtxos, datumHash) as Utxo;
  const collateralUtxo = getCollateralUtxo(paymentUtxos) as Utxo;
  const fractionUtxo = getFractionNftUtxo(paymentUtxos, fractionToken) as Utxo;

  const fractionAmount = fractionUtxo.amount.find(
    (a) => a.unit === fractionToken
  ) as Amount;

  // build redeemer
  await execa("cabal", ["run", "build-redeemer", "--", "Burn"], {
    cwd: config.OUT_DIR,
  });

  const policyId = await getPolicyIdByScript("payment");
  const tokenA = `1 ${policyId}.${asciiToHex(getTokenName(tokenAPrefix))}`;
  const tokenB = `1 ${policyId}.${asciiToHex(getTokenName(tokenBPrefix))}`;
  const fractionTokenUnit = `-${fractionAmount.quantity} ${fractionCurrencyId}.${fractionTokenName}`;
  const validityTokenUnit = `-1 ${fractionCurrencyId}.${asciiToHex(
    validityToken
  )}`;

  // prettier-ignore
  await execa("cardano-cli", [
    "transaction",
    "build",
    "--tx-in", `${fractionUtxo.txId}#${fractionUtxo.txIndex}`,
    "--tx-in", `${scriptUtxo.txId}#${scriptUtxo.txIndex}`,
    "--tx-in-script-file", `${config.OUT_DIR}/validator.plutus`,
    "--tx-in-datum-file", `${config.OUT_DIR}/datum.json`,
    "--tx-in-redeemer-file", "empty-redeemer.json",
    "--tx-in-collateral", `${collateralUtxo.txId}#${collateralUtxo.txIndex}`,
    "--tx-out", `${paymentAddr} + ${validatorLovelace} + ${tokenA} + ${tokenB}`,
    "--mint", `${fractionTokenUnit} + ${validityTokenUnit}`,
    "--mint-script-file", `${config.OUT_DIR}/minting.plutus`,
    "--mint-redeemer-file", `${config.OUT_DIR}/redeemer.json`,
    "--change-address", paymentAddr,
    "--protocol-params-file", `${config.OUT_DIR}/protocol-params.json`,
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
};

export const addNft = async () => {
  const datumHash = getDatumHash();
  const paymentAddr = getWalletAddress("payment");
  const validatorAddr = await getValidatorAddress();

  const paymentUtxos = await getUtxoByPaymentKey("payment");
  const validatorUtxos = await getValidatorUtxos(validatorAddr);
  const scriptUtxo = getScriptUtxo(validatorUtxos, datumHash) as Utxo;
  const secondTokenUtxo = getSecondTokenUtxo(paymentUtxos) as Utxo;
  const collateralUtxo = getCollateralUtxo(paymentUtxos) as Utxo;

  const policyId = await getPolicyIdByScript("payment");
  const fractionCurrencyId = getCurrencyId();
  const tokenA = `1 ${policyId}.${asciiToHex(getTokenName(tokenAPrefix))}`;
  const tokenB = `1 ${policyId}.${asciiToHex(getTokenName(tokenBPrefix))}`;
  const validityTokenUnit = `1 ${fractionCurrencyId}.${asciiToHex(
    validityToken
  )}`;

  // prettier-ignore
  await execa("cardano-cli", [
    "transaction",
    "build",
    ...networkFlagByNetworkId().split(" "),
    "--tx-in", `${scriptUtxo.txId}#${scriptUtxo.txIndex}`,
    "--tx-in-script-file", `${config.OUT_DIR}/validator.plutus`,
    "--tx-in-datum-file", `${config.OUT_DIR}/datum.json`,
    "--tx-in-redeemer-file", "empty-redeemer.json",
    "--tx-in-collateral", `${collateralUtxo.txId}#${collateralUtxo.txIndex}`,
    "--tx-in", `${secondTokenUtxo.txId}#${secondTokenUtxo.txIndex}`,
    "--tx-out", `${validatorAddr} + ${validatorLovelace} + ${tokenA} + ${tokenB} + ${validityTokenUnit}`,
    "--tx-out-datum-embed-file", `${config.OUT_DIR}/datum.json`,
    "--change-address", paymentAddr,
    ...[...Array(config.MIN_SIG).keys()].reduce(
      (acc, i) => [
        ...["--required-signer", `${config.OUT_DIR}/keys/signer_${i + 1}.skey`],
        ...acc,
      ],
      [] as string[]
    ),
    "--protocol-params-file", `${config.OUT_DIR}/protocol-params.json`,
    "--out-file", `${config.OUT_DIR}/tx.raw`,
  ]);

  // prettier-ignore
  await execa("cardano-cli", [
    "transaction",
    "sign",
    ...networkFlagByNetworkId().split(" "),
    "--tx-body-file", `${config.OUT_DIR}/tx.raw`,
    "--signing-key-file", `${config.OUT_DIR}/keys/payment.skey`,
    ...[...Array(config.MIN_SIG).keys()].reduce(
      (acc, i) => [
        ...["--signing-key-file", `${config.OUT_DIR}/keys/signer_${i + 1}.skey`],
        ...acc,
      ],
      [] as string[]
    ),
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
};
