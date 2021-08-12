import { ecdsaSign } from "secp256k1";
import { hexToBytes, toHex, hexToNumber } from "web3-utils";

import RollerRPCAPI from "../client/typescript/src/index";

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
    "0xa44de2416ee6beb2f323fab48b432925c9785808d33a6ca6d7ba00b45e9370c3",
  ],
  [
    "0xd53208cf45fc9bd7938b200bff8814a26146688f",
    "0x420b20f3538f7ddf4527770acbd33ed8aa858ba24eec5038bd22158f23a8a002",
  ],
  [
    "0x7b2a2d51e4d8fac602e20a5f6907ff9fbd88e1fd",
    "0x655eae6e301ebe9da6384f717f774f6addb165606a6990ce13e86ead710fff8b",
  ],
  [
    "0xf48062ae8bafd6ef19cd6cb89db93a0d0ca6ce26",
    "0x2480c5256d843c73cba67cc966a11a647c943a41db2fa138de4e4f16d0861a6b",
  ],
  [
    "0xf84a77aeb351c49dfa87e805a659d2daddff7606",
    "0xd6abd8fbab1db8714f1e284c11b8621cf95d0e319b4f38c54de4247f2150f1ba",
  ],
  [
    "0x167e357cf8b845370d0d408f9b389b66185b7b5b",
    "0x95f48754f44e6930473367a0802bdac7389e7749df2b3a6dd6e87bcbe0d0e0bc",
  ],
  [
    "0xcbecf3abc9878f07afc851aead2d8f1c436cc71d",
    "0x92596e42f9ee7a47e0d8c48291c768945fede98874cc250202a1f19f12c97be3",
  ],
  [
    "0x0afc0c3f4eeea500871f464ca71eef5e54a9af36",
    "0xa0ae1d77d89854a55a4abdc1300e989b1981728e8e669cfb4b4179f0af1ac389",
  ],
  [
    "0x6d654ef2489674d21aed428e8a4ad8ca4820f125",
    "0x7aec9f8027edaa2408ac5ca74b5ed929e271570a0eeed848f47bcee842902c16",
  ],
  [
    "0x218f6f87683db546ad47a5dc8b480e5a9b694866",
    "0x58d62eb79797502bc0f66cd3e7a49d00287bff53a2734b799ef09cb746340ed0",
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

const signHash = (hash: Hash, address: EthAddress) => {
  let msg = hexToBytes(hash).filter((d) => d !== 0);
  while (msg.length < 32) {
    msg.unshift(0);
  }
  console.log(hash, msg.length);
  return toHex(
    ecdsaSign(
      //  TODO: better way to remove leading zeroes from hash?
      //
      new Uint8Array(msg),
      new Uint8Array(hexToBytes(privateKeys.get(address)!))
    ).signature
  );
};

const starAddress: EthAddress = "0x6deffb0cafdb11d175f123f6891aa64f01c24f7d",
  spawnAddress: EthAddress = "0xf48062Ae8BAfD6Ef19CD6cb89db93A0d0ca6ce26",
  planetAddress: EthAddress = "0x6d654ef2489674d21aed428e8a4ad8ca4820f125",
  planetMgmtAddress: EthAddress = "0x218f6f87683db546ad47a5dc8b480e5a9b694866",
  sig: Signature = "0x1234", // We fake the signing in the backend
  fromStar: From = {
    ship: "~wanzod",
    proxy: "own",
  },
  fromBalhulTransfer: From = {
    ship: "~balhul-polsub",
    proxy: "transfer",
  },
  fromBalhulOwn: From = {
    ship: "~balhul-polsub",
    proxy: "own",
  },
  fromModlepTransfer: From = {
    ship: "~modlep-fosreg",
    proxy: "transfer",
  },
  fromModlepOwn: From = {
    ship: "~modlep-fosreg",
    proxy: "own",
  };

test("getRollerConfig", async () => {
  const config = await api.getRollerConfig();
  expect(config).toHaveProperty(
    "contract",
    "0xe6042703475d0dd1bc2eb564a55f1832c2527171"
  );
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
test("setSpawnProxy", async () => {
  // setSpawnProxy for ~wanzod
  //
  //    (assuming a clean L2 state, "sig" is signed using nonce 0)
  //
  const spawnProxyData = {
    address: starAddress,
  };
  //const wanzod: L2Point = await api.getPoint("~wanzod");
  //console.log(wanzod.ownership);
  const spanwProxyHash: Hash = await api.hashTransaction(
    0, //wanzod.ownership?.owner?.nonce!, // nonce = 0
    fromStar,
    "setSpawnProxy",
    spawnProxyData
  );
  console.log(spanwProxyHash);
  const proxyTxHash = await api.setSpawnProxy(
    signHash(spanwProxyHash, starAddress),
    fromStar,
    starAddress,
    spawnProxyData
  );
  expect(proxyTxHash).toBeTruthy();
  const txStatus = await api.getTransactionStatus(proxyTxHash);
  expect(txStatus).toEqual("pending");
});

test("spawn", async () => {
  // spawn ~modlep-fosreg
  //
  //    ("sig" is signed using nonce 1)
  //
  const spawnData = {
    address: spawnAddress,
    ship: "~modlep-fosreg",
  };
  const spawnHash: Hash = await api.hashTransaction(
    1, //wanzod.ownership?.owner?.nonce! + 1, // nonce = 1
    fromStar,
    "spawn",
    spawnData
  );
  const spawnTxHash = await api.spawn(
    signHash(spawnHash, starAddress),
    fromStar,
    starAddress,
    spawnData
  );
  expect(spawnTxHash).toBeTruthy();
  const txStatus = await api.getTransactionStatus(spawnTxHash);
  expect(txStatus).toEqual("pending");
});

test("transfer", async () => {
  // ~modlep-fosreg accepts the transfer...
  //
  //    ("sig" is signed using nonce 0)
  //
  const transferData = { address: planetAddress };
  //const modlep: L2Point = await api.getPoint("~modlep-fosreg");
  const transferHash: Hash = await api.hashTransaction(
    0,
    // ~modlep-fosreg is not spawned yet, since the txs are all pending
    //modlep.ownership?.transferProxy?.nonce!, // nonce = 0
    fromModlepTransfer,
    "transferPoint",
    transferData
  );

  console.log(transferHash);

  const transferTxHash = await api.transferPoint(
    signHash(transferHash, spawnAddress),
    fromModlepTransfer,
    spawnAddress,
    transferData
  );
  expect(transferTxHash).toBeTruthy();
  const txStatus = await api.getTransactionStatus(transferTxHash);
  expect(txStatus).toEqual("pending");
});

test("configureKeys", async () => {
  // ...configures keys
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
    //modlep.ownership?.owner?.nonce!, // nonce = 0
    fromModlepOwn,
    "configureKeys",
    configureKeysData
  );
  const configureTxHash = await api.configureKeys(
    signHash(configureKeysHash, planetAddress),
    fromModlepOwn,
    planetAddress,
    configureKeysData
  );
  expect(configureTxHash).toBeTruthy();
  const txStatus = await api.getTransactionStatus(configureTxHash);
  expect(txStatus).toEqual("pending");
});

test("setMgmtProxy", async () => {
  // ...and sets the management proxy so it's ready to boot
  //
  //    ("sig" is signed using nonce 1)
  //
  const mgmtData = { address: planetMgmtAddress };
  const mgmtHash: Hash = await api.hashTransaction(
    1,
    //modlep.ownership?.owner?.nonce! + 1, // nonce = 1
    fromModlepOwn,
    "setManagementProxy",
    mgmtData
  );
  const mgmtTxHash = await api.setManagementProxy(
    signHash(mgmtHash, planetAddress),
    fromModlepOwn,
    planetAddress,
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
// test("inviteFlowB", async () => {
//   // ~wanzod spawn ~balhul-polsub
//   //
//   //    (assuming no transactions have been submitted to the Roller
//   //     following the ones in inviteFlowA, "sig" is signed using nonce 2)
//   //
//   const spawnTxHash = await api.spawn(sig, fromStar, starAddress, {
//     address: spawnAddress,
//     ship: "~balhul-polsub",
//   });
//   expect(spawnTxHash).toBeTruthy();
//   const txStatus1 = await api.getTransactionStatus(spawnTxHash);
//   expect(txStatus1).toEqual("pending");

//   // ...sets the management proxy
//   //
//   //    ("sig" is signed using nonce 0)
//   //
//   const mgmtTxHash1 = await api.setManagementProxy(
//     sig,
//     fromBalhulOwn,
//     starAddress,
//     {
//       address: planetMgmtAddress,
//     }
//   );
//   expect(mgmtTxHash1).toBeTruthy();
//   const txStatus2 = await api.getTransactionStatus(mgmtTxHash1);
//   expect(txStatus2).toEqual("pending");

//   // ...and configures keys  so it's ready to boot
//   //
//   //    ("sig" is signed using nonce 1)
//   //
//   const configureTxHash1 = await api.configureKeys(
//     sig,
//     fromBalhulOwn,
//     starAddress,
//     {
//       encrypt: "0x1234",
//       auth: "0xabcd",
//       cryptoSuite: 0,
//       breach: false,
//     }
//   );
//   expect(configureTxHash1).toBeTruthy();
//   const txStatus3 = await api.getTransactionStatus(configureTxHash1);
//   expect(txStatus3).toEqual("pending");

//   // When the Roller submits the transactions up to this point to an Ethereum node, and
//   // they get confirmed on L1, ~balhul-polsub is ready to boot inmediately, but ~wanzod
//   // still controls the planet (the ownership proxy of ~balhul-polsub is set to ~wanzod's
//   // ownership proxy)
//   //
//   // In order for ~balhul-polsub to gain full control/ownership the following is needed

//   // ~balhul-polsub accepts the transfer, resetting all proxies
//   //
//   //    ("sig" is signed using nonce 0)
//   //
//   const transferTxHash = await api.transferPoint(
//     sig,
//     fromBalhulTransfer,
//     spawnAddress,
//     { address: planetAddress, reset: true }
//   );
//   expect(transferTxHash).toBeTruthy();
//   const txStatus4 = await api.getTransactionStatus(transferTxHash);
//   expect(txStatus4).toEqual("pending");

//   // ...sets a new management proxy
//   //
//   //    ("sig" is signed using nonce 2)
//   //
//   const mgmtTxHash2 = await api.setManagementProxy(
//     sig,
//     fromBalhulOwn,
//     planetAddress,
//     {
//       address: "0x0afc0c3f4eeea500871f464ca71eef5e54a9af36",
//     }
//   );
//   expect(mgmtTxHash2).toBeTruthy();
//   const txStatus5 = await api.getTransactionStatus(mgmtTxHash2);
//   expect(txStatus5).toEqual("pending");

//   // ...and configures keys so it's ready to boot
//   //
//   //    ("sig" is signed using nonce 3)
//   //
//   const configureTxHash2 = await api.configureKeys(
//     sig,
//     fromBalhulOwn,
//     planetAddress,
//     {
//       encrypt: "0x5678",
//       auth: "0xefab",
//       cryptoSuite: 0,
//       breach: false,
//     }
//   );
//   expect(configureTxHash2).toBeTruthy();
//   const txStatus6 = await api.getTransactionStatus(configureTxHash2);
//   expect(txStatus6).toEqual("pending");
// });
