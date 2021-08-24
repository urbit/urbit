import { ecdsaSign } from "secp256k1";
import keccak, { Keccak } from "keccak";
import { hexToBytes, toHex } from "web3-utils";

import RollerRPCAPI, { L2Data } from "../client/typescript/src/index";

import type {
  L2Point,
  EthAddress,
  Hash,
  Ship,
  Signature,
  From,
} from "../client/typescript/src/index";

const privateKeys = new Map([
  [
    "0x6deffb0cafdb11d175f123f6891aa64f01c24f7d",
    "a44de2416ee6beb2f323fab48b432925c9785808d33a6ca6d7ba00b45e9370c3",
  ],
  [
    "0xd53208cf45fc9bd7938b200bff8814a26146688f",
    "420b20f3538f7ddf4527770acbd33ed8aa858ba24eec5038bd22158f23a8a002",
  ],
  [
    "0x7b2a2d51e4d8fac602e20a5f6907ff9fbd88e1fd",
    "655eae6e301ebe9da6384f717f774f6addb165606a6990ce13e86ead710fff8b",
  ],
  [
    "0xf48062ae8bafd6ef19cd6cb89db93a0d0ca6ce26",
    "2480c5256d843c73cba67cc966a11a647c943a41db2fa138de4e4f16d0861a6b",
  ],
  [
    "0xf84a77aeb351c49dfa87e805a659d2daddff7606",
    "d6abd8fbab1db8714f1e284c11b8621cf95d0e319b4f38c54de4247f2150f1ba",
  ],
  [
    "0x167e357cf8b845370d0d408f9b389b66185b7b5b",
    "95f48754f44e6930473367a0802bdac7389e7749df2b3a6dd6e87bcbe0d0e0bc",
  ],
  [
    "0xcbecf3abc9878f07afc851aead2d8f1c436cc71d",
    "92596e42f9ee7a47e0d8c48291c768945fede98874cc250202a1f19f12c97be3",
  ],
  [
    "0x0afc0c3f4eeea500871f464ca71eef5e54a9af36",
    "a0ae1d77d89854a55a4abdc1300e989b1981728e8e669cfb4b4179f0af1ac389",
  ],
  [
    "0x6d654ef2489674d21aed428e8a4ad8ca4820f125",
    "7aec9f8027edaa2408ac5ca74b5ed929e271570a0eeed848f47bcee842902c16",
  ],
  [
    "0x218f6f87683db546ad47a5dc8b480e5a9b694866",
    "58d62eb79797502bc0f66cd3e7a49d00287bff53a2734b799ef09cb746340ed0",
  ],
]);

const api = new RollerRPCAPI({
  transport: {
    type: "http",
    host: "localhost",
    port: 8080,
    path: "/v1/roller",
  },
});

const signMessage = (msg: Hash, address: EthAddress) => {
  //  msg is a keccak-256 hash
  //
  const hashed = Buffer.from(hexToBytes(msg));
  const prvKey = Buffer.from(privateKeys.get(address)!, "hex");
  const { signature, recid } = ecdsaSign(hashed, prvKey);
  // add key recovery parameter
  const ethSignature = new Uint8Array(65);
  ethSignature.set(signature);
  ethSignature[64] = recid;
  return toHex(Buffer.from(ethSignature));
};

const starAddress: EthAddress = "0x6deffb0cafdb11d175f123f6891aa64f01c24f7d";
const planetReceivingAddress: EthAddress =
  "0xf48062ae8bafd6ef19cd6cb89db93a0d0ca6ce26";
const planetOwnershipAddress: EthAddress =
  "0x6d654ef2489674d21aed428e8a4ad8ca4820f125";
const planetMgmtAddress: EthAddress =
  "0x218f6f87683db546ad47a5dc8b480e5a9b694866";
const fromStar: From = {
  ship: "~wanzod",
  proxy: "own",
};
const fromBalhulTransfer: From = {
  ship: "~balhul-polsub",
  proxy: "transfer",
};
const fromBalhulOwn: From = {
  ship: "~balhul-polsub",
  proxy: "own",
};
const fromModlepTransfer: From = {
  ship: "~modlep-fosreg",
  proxy: "transfer",
};
const fromModlepOwn: From = {
  ship: "~modlep-fosreg",
  proxy: "own",
};

test("getRollerConfig", async () => {
  const config = await api.getRollerConfig();
  expect(config).toHaveProperty("contract");
});

test("getPoint", async () => {
  const ship = "~norsyr-torryn";
  const point: L2Point = await api.getPoint(ship);
  expect(point).toHaveProperty("dominion", "l2");
});

test("getShips", async () => {
  const address: EthAddress = "0x6deffb0cafdb11d175f123f6891aa64f01c24f7d";
  const ships: Ship[] = await api.getShips(address);
  expect(ships.length).toBeGreaterThan(0);
});

// L2 Invite Flow - A -
//
//   The spawned planet configures keys and sets its management proxy
//
//   Example Postman collection:
//   https://documenter.getpostman.com/view/16338962/Tzm3nx7x#f01fbd47-3f2c-413c-a922-3c964c5bb65c
//
test("~wanzod sets its spawn proxy", async () => {
  //
  //    (assuming a clean L2 state, "sig" is signed using nonce 0)
  //
  const spawnProxyData = {
    address: starAddress,
  };
  const wanzod: L2Point = await api.getPoint("~wanzod");
  const nonce = wanzod.ownership?.owner?.nonce;

  const spanwProxyHash: Hash = await api.hashTransaction(
    nonce ? nonce : 0, // nonce = 0
    fromStar,
    "setSpawnProxy",
    spawnProxyData
  );
  const proxyTxHash = await api.setSpawnProxy(
    signMessage(spanwProxyHash, starAddress),
    fromStar,
    starAddress,
    spawnProxyData
  );
  expect(proxyTxHash).toBeTruthy();
  const txStatus = await api.getTransactionStatus(proxyTxHash);
  expect(txStatus).toEqual("pending");
});

test("and spawns ~modlep-fosreg", async () => {
  //
  //    ("sig" is signed using nonce 1)
  //
  const spawnData = {
    address: planetReceivingAddress,
    ship: "~modlep-fosreg",
  };
  const wanzod: L2Point = await api.getPoint("~wanzod");
  const nonce = wanzod.ownership?.owner?.nonce;

  const spawnHash: Hash = await api.hashTransaction(
    nonce ? nonce + 1 : 1, // nonce = 1
    fromStar,
    "spawn",
    spawnData
  );

  const spawnTxHash = await api.spawn(
    signMessage(spawnHash, starAddress),
    fromStar,
    starAddress,
    spawnData
  );
  expect(spawnTxHash).toBeTruthy();
  const txStatus = await api.getTransactionStatus(spawnTxHash);
  expect(txStatus).toEqual("pending");
});

test("~modlep-fosreg accepts the transfer", async () => {
  //
  //    ("sig" is signed using nonce 0)
  //
  const transferData = { address: planetOwnershipAddress, reset: true };
  // const modlep: L2Point = await api.getPoint("~modlep-fosreg");
  const transferHash: Hash = await api.hashTransaction(
    0,
    // ~modlep-fosreg is not spawned yet, since the txs are all pending
    // modlep.ownership?.transferProxy?.nonce!, // nonce = 0
    fromModlepTransfer,
    "transferPoint",
    transferData
  );

  const transferTxHash = await api.transferPoint(
    signMessage(transferHash, planetReceivingAddress),
    fromModlepTransfer,
    planetReceivingAddress,
    transferData
  );
  expect(transferTxHash).toBeTruthy();
  const txStatus = await api.getTransactionStatus(transferTxHash);
  expect(txStatus).toEqual("pending");
});

test("...configures keys", async () => {
  //
  //    ("sig" is signed using nonce 0)
  //
  const configureKeysData = {
    encrypt: "0x1234",
    auth: "0xabcd",
    cryptoSuite: 0,
    breach: false,
  };
  const configureKeysHash: Hash = await api.hashTransaction(
    0,
    // modlep.ownership?.owner?.nonce!, // nonce = 0
    fromModlepOwn,
    "configureKeys",
    configureKeysData
  );

  const configureTxHash = await api.configureKeys(
    signMessage(configureKeysHash, planetOwnershipAddress),
    fromModlepOwn,
    planetOwnershipAddress,
    configureKeysData
  );
  expect(configureTxHash).toBeTruthy();
  const txStatus = await api.getTransactionStatus(configureTxHash);
  expect(txStatus).toEqual("pending");
});

test("and sets management proxy", async () => {
  // ...and sets the management proxy so it's ready to boot
  //
  //    ("sig" is signed using nonce 1)
  //
  const mgmtData = { address: planetMgmtAddress };
  const mgmtHash: Hash = await api.hashTransaction(
    1,
    // modlep.ownership?.owner?.nonce! + 1, // nonce = 1
    fromModlepOwn,
    "setManagementProxy",
    mgmtData
  );

  const mgmtTxHash = await api.setManagementProxy(
    signMessage(mgmtHash, planetOwnershipAddress),
    fromModlepOwn,
    planetOwnershipAddress,
    mgmtData
  );
  expect(mgmtTxHash).toBeTruthy();
  const txStatus5 = await api.getTransactionStatus(mgmtTxHash);
  expect(txStatus5).toEqual("pending");
});

// L2 Invite Flow - B -
//
//   The sponsor star configures keys and sets the planet's management proxy
//
//   Example Postman collection:
//   https://documenter.getpostman.com/view/16338962/Tzm3nx7x#7b060cd8-e161-4a09-86f0-b1ffcb111d08
//
test("~wanzod spawns ~balhul-polsub", async () => {
  //
  //    (assuming no transactions have been submitted to the Roller
  //     following the ones in inviteFlowA, "sig" is signed using nonce 2)
  //
  const spawnData = {
    address: planetReceivingAddress,
    ship: "~balhul-polsub",
  };

  const spawnHash: Hash = await api.hashTransaction(
    2,
    fromStar,
    "spawn",
    spawnData
  );
  const spawnTxHash = await api.spawn(
    signMessage(spawnHash, starAddress),
    fromStar,
    starAddress,
    spawnData
  );
  expect(spawnTxHash).toBeTruthy();
  const txStatus = await api.getTransactionStatus(spawnTxHash);
  expect(txStatus).toEqual("pending");
});

test("...sets its management proxy", async () => {
  //
  //    ("sig" is signed using nonce 0)
  //
  const mgmtData = { address: planetMgmtAddress };
  const mgmtHash: Hash = await api.hashTransaction(
    0,
    fromBalhulOwn,
    "setManagementProxy",
    mgmtData
  );

  const mgmtTxHash = await api.setManagementProxy(
    signMessage(mgmtHash, starAddress),
    fromBalhulOwn,
    starAddress,
    mgmtData
  );

  expect(mgmtTxHash).toBeTruthy();
  const txStatus = await api.getTransactionStatus(mgmtTxHash);
  expect(txStatus).toEqual("pending");
});

test("and configures its keys", async () => {
  //
  //    ("sig" is signed using nonce 1)
  //

  const configureKeysData = {
    encrypt: "0x1234",
    auth: "0xabcd",
    cryptoSuite: 0,
    breach: false,
  };
  const configureKeysHash: Hash = await api.hashTransaction(
    1,
    fromBalhulOwn,
    "configureKeys",
    configureKeysData
  );

  const configureTxHash = await api.configureKeys(
    signMessage(configureKeysHash, starAddress),
    fromBalhulOwn,
    starAddress,
    configureKeysData
  );
  expect(configureTxHash).toBeTruthy();
  const txStatus = await api.getTransactionStatus(configureTxHash);
  expect(txStatus).toEqual("pending");
});

// When the Roller submits the transactions up to this point to an Ethereum node, and
// they get confirmed on L1, ~balhul-polsub is ready to boot inmediately, but ~wanzod
// still controls the planet (the ownership proxy of ~balhul-polsub is set to ~wanzod's
// ownership proxy)
//
// In order for ~balhul-polsub to gain full control/ownership the following is needed

test("~balhul-polsub accepts the transfer", async () => {
  //
  //    ("sig" is signed using nonce 0)
  //

  const transferData = { address: planetOwnershipAddress, reset: true };
  const transferHash: Hash = await api.hashTransaction(
    0,
    fromBalhulTransfer,
    "transferPoint",
    transferData
  );

  const transferTxHash = await api.transferPoint(
    signMessage(transferHash, planetReceivingAddress),
    fromBalhulTransfer,
    planetReceivingAddress,
    transferData
  );

  expect(transferTxHash).toBeTruthy();
  const txStatus = await api.getTransactionStatus(transferTxHash);
  expect(txStatus).toEqual("pending");
});

test("... sets a new management proxy", async () => {
  //
  //    ("sig" is signed using nonce 2)
  //

  const mgmtData = { address: "0x0afc0c3f4eeea500871f464ca71eef5e54a9af36" };
  const mgmtHash: Hash = await api.hashTransaction(
    2,
    fromBalhulOwn,
    "setManagementProxy",
    mgmtData
  );

  const mgmtTxHash = await api.setManagementProxy(
    signMessage(mgmtHash, planetOwnershipAddress),
    fromBalhulOwn,
    planetOwnershipAddress,
    mgmtData
  );

  expect(mgmtTxHash).toBeTruthy();
  const txStatus = await api.getTransactionStatus(mgmtTxHash);
  expect(txStatus).toEqual("pending");
});

test("and configures new keys", async () => {
  // ...and configures keys so it's ready to boot
  //
  //    ("sig" is signed using nonce 3)
  //

  const configureKeysData = {
    encrypt: "0x5678",
    auth: "0xefab",
    cryptoSuite: 0,
    breach: false,
  };
  const configureKeysHash: Hash = await api.hashTransaction(
    3,
    fromBalhulOwn,
    "configureKeys",
    configureKeysData
  );

  const configureTxHash = await api.configureKeys(
    signMessage(configureKeysHash, planetOwnershipAddress),
    fromBalhulOwn,
    planetOwnershipAddress,
    configureKeysData
  );

  expect(configureTxHash).toBeTruthy();
  const txStatus = await api.getTransactionStatus(configureTxHash);
  expect(txStatus).toEqual("pending");
});
