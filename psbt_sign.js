/*
Usage:
const p = require("./psbt_sign.js")
const psbt = PSBT_STRING_BASE64
const mnemonics = MNEMONICS_STRING
p.run(mnemonics, psbt)
  */

const bitcoin = require('bitcoinjs-lib');
const ecpair = bitcoin.ECPair;
const crypto = bitcoin.crypto;
const bscript = bitcoin.script;
const bip39 = require('bip39');
const NETWORK = bitcoin.networks.bitcoin;
const LOCK_TIME = 0;
const SIGHASH_ALL = 1;

const isSegwitTx = (rawTx) => {
    return rawTx.substring(8, 12) === '0001';
};

const PATHS = ["m/84'/0'/0'/0/0", "m/84'/0'/0'/1/0"];

const run = async(mnemonics, psbtStr) => {
    const psbt = bitcoin.Psbt.fromBase64(psbtStr);
    const seed = await bip39.mnemonicToSeed(mnemonics);
    const node = bitcoin.bip32.fromSeed(seed, NETWORK);
    const signers = PATHS.map((p) => node.derivePath(p));
    const publicKeys = signers.map((s) => s.publicKey);
    const privateKeys = signers.map((s) => s.privateKey);

    psbt.signInput(0, bitcoin.ECPair.fromPrivateKey(privateKeys[0]));
    const validate = await psbt.validateSignaturesOfAllInputs();
    await psbt.finalizeAllInputs();
    const hex = psbt.extractTransaction().toHex();
    console.log({ validate, hex});

    return;
};

exports.run = run;
