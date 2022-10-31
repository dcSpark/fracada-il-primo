/*
 *
 * Requirements to run this script: cabal, ghc, cardano-cli, cardano-node (fully syncd and running)
 *
 */

import {
  bootstrapWallet,
  generatePaymentKeys,
  getBalanceValueFromCliUtxos,
  getPolicyIdByScript as getPolicyIdByKey,
  getTokenName,
  getUtxoByPaymentKey,
  getValidatorAddress,
  getWalletAddress as getWalletAddressByKey,
  initializeValidatorScript,
  isPaymentKeysExist,
  isValidatorScriptInitialized,
  sleep,
  stringFormatBalance,
  waitForTxConf,
} from "./utils";
import { addNft, lockNft, unlockNft } from "./endpoints";
import { createLogger, transports, format } from "winston";
import { tokenAPrefix, tokenBPrefix } from "./constants";

const Main = async () => {
  logger.info("Welcome to Fracada Testnet Deployment Script");

  await generateKeys();
  await compileScript();
  await waitForBalance();

  // Testing scenario
  if (!(await bootstrap())) return;
  if (!(await fractionalize())) return;
  if (!(await addAnotherToken())) return;
  if (!(await returnTokens())) return;
};

const generateKeys = async () => {
  if (!isPaymentKeysExist()) {
    logger.warn(`Payment keypair not found, generating new keypair...`);
    await generatePaymentKeys();
  }

  logger.info(`Payment Address: ${getWalletAddressByKey("payment")}`);
  const policyId = await getPolicyIdByKey("payment");
  logger.info(`Tokens PolicyId: ${policyId}`);
};

const compileScript = async () => {
  if (!isValidatorScriptInitialized()) {
    logger.warn("Plutus Script not detected, Initializating...");
    await initializeValidatorScript();
    logger.info("Plutus Script Initialized!");
  }

  const validatorAddr = await getValidatorAddress();
  logger.info(`Contract Address: ${validatorAddr}`);
};

const waitForBalance = async () => {
  while (true) {
    const utxos = await getUtxoByPaymentKey("payment");

    const balance = getBalanceValueFromCliUtxos(utxos);
    if (balance["lovelace"] === undefined || balance["lovelace"] < 50_000_000) {
      logger.warn(
        "Wallet balance is less than 50,000,000 Lovelace, please send some lovelaces to the payment wallet"
      );
      await sleep(10_000);
    } else {
      logger.info(`\n[Wallet Balance] \n${stringFormatBalance(balance)}`);
      break;
    }
  }
};

const bootstrap = async () => {
  logger.info("Bootstrapping wallet for Fracada end to end test...");
  const bootstrapTxId = await bootstrapWallet();
  if (!bootstrapTxId) {
    logger.error("Error bootstrapping wallet.");
    return false;
  }

  logger.info(`Bootstrap txId: ${bootstrapTxId}`);
  logger.info(`Waiting for confirmation...`);
  await waitForTxConf(bootstrapTxId);

  const utxos = await getUtxoByPaymentKey("payment");
  const balance = getBalanceValueFromCliUtxos(utxos);
  logger.info(`\n[Wallet Balance] \n${stringFormatBalance(balance)}`);

  return true;
};

const fractionalize = async () => {
  logger.info(`Fractionalizing ${getTokenName(tokenAPrefix)}...`);
  const lockTxId = await lockNft();
  if (!lockTxId) {
    logger.error(`Error fractionalizing ${getTokenName(tokenAPrefix)}.`);
    return false;
  }

  logger.info(`Fractionlization txId: ${lockTxId}`);
  logger.info(`Waiting for confirmation...`);

  await waitForTxConf(lockTxId);
  logger.info("Fractionalization Complete!");

  const utxos = await getUtxoByPaymentKey("payment");
  const balance = getBalanceValueFromCliUtxos(utxos);
  logger.info(`\n[Wallet Balance] \n${stringFormatBalance(balance)}`);

  return true;
};

const addAnotherToken = async () => {
  logger.info(`Adding ${getTokenName(tokenBPrefix)} to the contract...`);
  const addNftTxId = await addNft();
  if (!addNftTxId) {
    logger.error(`Error adding ${getTokenName(tokenBPrefix)}.`);
    return false;
  }

  logger.info(`AddNft txId: ${addNftTxId}`);
  logger.info(`Waiting for confirmation...`);
  await waitForTxConf(addNftTxId);

  logger.info(`${getTokenName(tokenBPrefix)} added to the contract!`);

  const utxos = await getUtxoByPaymentKey("payment");
  const balance = getBalanceValueFromCliUtxos(utxos);
  logger.info(`\n[Wallet Balance] \n${stringFormatBalance(balance)}`);

  return true;
};

const returnTokens = async () => {
  logger.info("Unlocking tokens...");
  const unlockTxId = await unlockNft();
  if (!unlockTxId) {
    logger.error("Error unlocking tokens.");
    return false;
  }

  logger.info(`Unlock txId: ${unlockTxId}`);
  logger.info(`Waiting for confirmation...`);
  await waitForTxConf(unlockTxId);

  logger.info("Unlocking Complete!");

  const utxos = await getUtxoByPaymentKey("payment");
  const balance = getBalanceValueFromCliUtxos(utxos);
  logger.info(`\n[Wallet Balance] \n${stringFormatBalance(balance)}`);

  return true;
};

const logger = createLogger({
  format: format.combine(
    format.colorize({ all: true }),
    format.timestamp({ format: "YYYY/MM/DD HH:mm:ss" }),
    format.printf(
      (info) => `[${info.timestamp}] ${info.level}: ${info.message}`
    )
  ),
  transports: [new transports.Console()],
});

Main();
