const bitcoin = require('bitcoinjs-lib');
const ecpair = bitcoin.ECPair;
const crypto = bitcoin.crypto;
const bscript = bitcoin.script;
const bip39 = require('bip39');
const LOCK_TIME = 0;
const SIGHASH_ALL = 1;
let NETWORK;

/*
Usage:
var p = require("./psbt_sign.js")
var psbt = PSBT_STRING_BASE64
const mnemonics = MNEMONICS_STRING

p.run("TESTNET", mnemonics, psbt)
p.run("MAIN", mnemonics, psbt)
  */

const isSegwitTx = (rawTx) => {
    return rawTx.substring(8, 12) === '0001';
};

//  const PATHS = ["m/84'/0'/0'/0/0", "m/84'/0'/0'/1/0"];

const run = async(network, mnemonics, psbtStr) => {
    let NETWORK;
    if (network === "MAIN") {
        NETWORK = bitcoin.networks.bitcoin;
    }
    else if (network = "TESTNET") {
        NETWORK = bitcoin.networks.testnet;
    }

    const psbt = bitcoin.Psbt.fromBase64(psbtStr);
    const is = psbt.data.inputs;
    const seed = await bip39.mnemonicToSeed(mnemonics);
    const node = bitcoin.bip32.fromSeed(seed, NETWORK);

    for(let i=0; i<is.length; i++) {
        const der = is[i].bip32Derivation[0];
        const walletDer = node.derivePath(der.path);

        if(0 === Buffer.compare(der.pubkey, walletDer.publicKey)) {
            console.log(`Signing ${der.path}`);
            psbt.signInput(i, bitcoin.ECPair.fromPrivateKey(walletDer.privateKey));
        }
        const validate = await psbt.validateSignaturesOfAllInputs();
        await psbt.finalizeAllInputs();
        const hex = psbt.extractTransaction().toHex();
        console.log(bitcoin.Transaction.fromHex(hex).getId())
        console.log({ validate, hex});
    }
    return;
};

exports.run = run;
