/*
 *
 * Requirements to run this script: cabal, ghc, cardano-cli, cardano-node (fully syncd and running)
 *
 */

import { config } from './config';
import {
  bootstrapWallet,
  generatePaymentKeys,
  getBalanceValueFromCliUtxos,
  getPolicyIdByScript as getPolicyIdByKey,
  getUtxoByPaymentKey,
  getValidatorAddress,
  getValidatorUtxos,
  getWalletAddress as getWalletAddressByKey,
  initializeValidatorScript,
  isPaymentKeysExist,
  isValidatorScriptInitialized,
  lockNft,
  sleep,
  stringFormatBalance,
  unlockNft,
  Utxo,
  waitForTxConf
} from './utils';
import { createLogger, transports, format } from 'winston';

const Main = async () => {
  const validatorAddr = await getValidatorAddress(config.OUT_DIR, config.NETWORK_ID);

  logger.info("Welcome to Fracada Testnet Deployment Script");

  if (!isPaymentKeysExist(config.OUT_DIR)) {
    logger.warn(`Payment keypair not found, generating new keypair...`);
    await generatePaymentKeys(config.OUT_DIR, config.NETWORK_ID);
  }

  logger.info(`Contract Address: ${validatorAddr}`);
  logger.info(`Payment Address: ${await getWalletAddressByKey(config.OUT_DIR, 'payment')}`);
  const fakeNftPolicyId = await getPolicyIdByKey(config.OUT_DIR, 'payment');
  logger.info(`Fake NFT PolicyId: ${fakeNftPolicyId}`);

  // Check if validator script is already initialized
  if (!isValidatorScriptInitialized(config.OUT_DIR)) {
    logger.warn('Plutus Script not detected, Initializating paramterization...');
    await initializeValidatorScript(fakeNftPolicyId, config.OUT_DIR, config.MIN_SIG);
    logger.info('Plutus Script Initialized!');
  }

  // Display Balance
  let utxos = [] as Utxo[];
  let balance = {} as any;

  while (true) {
    utxos = await getUtxoByPaymentKey(config.OUT_DIR, 'payment', config.NETWORK_ID);
    balance = getBalanceValueFromCliUtxos(utxos);
    if (balance['lovelace'] === undefined || balance['lovelace'] < 50_000_000) {
      logger.warn('Wallet balance is less than 50,000,000 Lovelace, please send some lovelaces to the payment wallet');
      await sleep(10_000);
    } else break;
  }

  logger.info(`\n[Wallet Balance] \n${stringFormatBalance(balance)}`);

  // Bootstrap Wallet
  logger.info('Bootrapping wallet for Fracada end to end test...');
  const bootstrapTxId = await bootstrapWallet(config.OUT_DIR, config.NETWORK_ID);

  if (bootstrapTxId !== undefined) {
    logger.info(`Bootstrap txId: ${bootstrapTxId}`);
    logger.info(`Waiting for confirmation...`);
    await waitForTxConf(bootstrapTxId, config.NETWORK_ID);
  }

  utxos = await getUtxoByPaymentKey(config.OUT_DIR, 'payment', config.NETWORK_ID);
  balance = getBalanceValueFromCliUtxos(utxos);
  logger.info(`\n[Wallet Balance] \n${stringFormatBalance(balance)}`);

  logger.info('Fractionalizing FakeNft_A...');
  const lockTxId = await lockNft(config.OUT_DIR, config.NETWORK_ID);
  if (lockTxId !== undefined) {
    logger.info(`Fractionlization txId: ${lockTxId}`);
    logger.info(`Waiting for confirmation...`);
    await waitForTxConf(lockTxId, config.NETWORK_ID);
  }

  logger.info('Fractionalization Complete!');

  utxos = await getUtxoByPaymentKey(config.OUT_DIR, 'payment', config.NETWORK_ID);
  balance = getBalanceValueFromCliUtxos(utxos);
  logger.info(`\n[Wallet Balance] \n${stringFormatBalance(balance)}`);

  logger.info('Unlocking FakeNFT_A...');
  const unlockTxId = await unlockNft(config.OUT_DIR, config.NETWORK_ID);

  if (unlockTxId !== undefined) {
    logger.info(`Unlock txId: ${unlockTxId}`);
    logger.info(`Waiting for confirmation...`);
    await waitForTxConf(unlockTxId, config.NETWORK_ID);
  }

  logger.info('Unlocking Complete!');

  utxos = await getUtxoByPaymentKey(config.OUT_DIR, 'payment', config.NETWORK_ID);
  balance = getBalanceValueFromCliUtxos(utxos);
  logger.info(`\n[Wallet Balance] \n${stringFormatBalance(balance)}`);

}

const logger = createLogger({
  format: format.combine(
    format.colorize({ all: true }),
    format.timestamp({ format: 'YYYY/MM/DD HH:mm:ss' }),
    format.printf(info => `[${info.timestamp}] ${info.level}: ${info.message}`)
  ),
  transports: [
    new transports.Console()
  ]
});

Main();