import RollerRPCAPI from "../client/typescript/src/index";

import type {
  L2Point,
  EthAddress,
  Hash,
  Ship,
  Signature,
  From,
} from "../client/typescript/src/index";

const api = new RollerRPCAPI({
  transport: {
    type: "http",
    host: "localhost",
    port: 8080,
    path: "/v1/roller",
  },
});

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
// L2 Invite Flow - A -
//
//   The spawned planet configures keys and sets its management proxy
//
//   Example Postman collection:
//   https://documenter.getpostman.com/view/16338962/Tzm3nx7x#f01fbd47-3f2c-413c-a922-3c964c5bb65c
//
test("inviteFlowA", async () => {
  // setSpawnProxy for ~wanzod
  //
  //    (assuming a clean L2 state, "sig" is signed using nonce 0)
  //
  const proxyTxHash = await api.setSpawnProxy(sig, fromStar, starAddress, {
    address: starAddress,
  });
  expect(proxyTxHash).toBeTruthy();
  const txStatus1 = await api.getTransactionStatus(proxyTxHash);
  expect(txStatus1).toEqual("pending");

  // spawn ~modlep-fosreg
  //
  //    ("sig" is signed using nonce 1)
  //
  const spawnTxHash = await api.spawn(sig, fromStar, starAddress, {
    address: spawnAddress,
    ship: "~modlep-fosreg",
  });
  expect(spawnTxHash).toBeTruthy();
  const txStatus2 = await api.getTransactionStatus(spawnTxHash);
  expect(txStatus2).toEqual("pending");

  // ~modlep-fosreg accepts the transfer...
  //
  //    ("sig" is signed using nonce 0)
  //
  const transferTxHash = await api.transferPoint(
    sig,
    fromModlepTransfer,
    spawnAddress,
    { address: planetAddress }
  );
  expect(transferTxHash).toBeTruthy();
  const txStatus3 = await api.getTransactionStatus(transferTxHash);
  expect(txStatus3).toEqual("pending");

  // ...configures keys
  //
  //    ("sig" is signed using nonce 0)
  //
  const configureTxHash = await api.configureKeys(
    sig,
    fromModlepOwn,
    planetAddress,
    { encrypt: "0x1234", auth: "0xabcd", cryptoSuite: 0, breach: false }
  );
  expect(configureTxHash).toBeTruthy();
  const txStatus4 = await api.getTransactionStatus(transferTxHash);
  expect(txStatus4).toEqual("pending");

  // ...and sets the management proxy so it's ready to boot
  //
  //    ("sig" is signed using nonce 1)
  //
  const mgmtTxHash = await api.setManagementProxy(
    sig,
    fromModlepOwn,
    planetAddress,
    { address: planetMgmtAddress }
  );
  expect(mgmtTxHash).toBeTruthy();
  const txStatus5 = await api.getTransactionStatus(transferTxHash);
  expect(txStatus5).toEqual("pending");
});

// L2 Invite Flow - B -
//
//   The sponsor star configures keys and sets the planet's management proxy
//
//   Example Postman collection:
//   https://documenter.getpostman.com/view/16338962/Tzm3nx7x#7b060cd8-e161-4a09-86f0-b1ffcb111d08
//
test("inviteFlowB", async () => {
  // ~wanzod spawn ~balhul-polsub
  //
  //    (assuming no transactions have been submitted to the Roller
  //     following the ones in inviteFlowA, "sig" is signed using nonce 2)
  //
  const spawnTxHash = await api.spawn(sig, fromStar, starAddress, {
    address: spawnAddress,
    ship: "~balhul-polsub",
  });
  expect(spawnTxHash).toBeTruthy();
  const txStatus1 = await api.getTransactionStatus(spawnTxHash);
  expect(txStatus1).toEqual("pending");

  // ...sets the management proxy
  //
  //    ("sig" is signed using nonce 0)
  //
  const mgmtTxHash1 = await api.setManagementProxy(
    sig,
    fromBalhulOwn,
    starAddress,
    {
      address: planetMgmtAddress,
    }
  );
  expect(mgmtTxHash1).toBeTruthy();
  const txStatus2 = await api.getTransactionStatus(mgmtTxHash1);
  expect(txStatus2).toEqual("pending");

  // ...and configures keys  so it's ready to boot
  //
  //    ("sig" is signed using nonce 1)
  //
  const configureTxHash1 = await api.configureKeys(
    sig,
    fromBalhulOwn,
    starAddress,
    {
      encrypt: "0x1234",
      auth: "0xabcd",
      cryptoSuite: 0,
      breach: false,
    }
  );
  expect(configureTxHash1).toBeTruthy();
  const txStatus3 = await api.getTransactionStatus(configureTxHash1);
  expect(txStatus3).toEqual("pending");

  // When the Roller submits the transactions up to this point to an Ethereum node, and
  // they get confirmed on L1, ~balhul-polsub is ready to boot inmediately, but ~wanzod
  // still controls the planet (the ownership proxy of ~balhul-polsub is set to ~wanzod's
  // ownership proxy)
  //
  // In order for ~balhul-polsub to gain full control/ownership the following is needed

  // ~balhul-polsub accepts the transfer, resetting all proxies
  //
  //    ("sig" is signed using nonce 0)
  //
  const transferTxHash = await api.transferPoint(
    sig,
    fromBalhulTransfer,
    spawnAddress,
    { address: planetAddress, reset: true }
  );
  expect(transferTxHash).toBeTruthy();
  const txStatus4 = await api.getTransactionStatus(transferTxHash);
  expect(txStatus4).toEqual("pending");

  // ...sets a new management proxy
  //
  //    ("sig" is signed using nonce 2)
  //
  const mgmtTxHash2 = await api.setManagementProxy(
    sig,
    fromBalhulOwn,
    planetAddress,
    {
      address: "0x0afc0c3f4eeea500871f464ca71eef5e54a9af36",
    }
  );
  expect(mgmtTxHash2).toBeTruthy();
  const txStatus5 = await api.getTransactionStatus(mgmtTxHash2);
  expect(txStatus5).toEqual("pending");

  // ...and configures keys so it's ready to boot
  //
  //    ("sig" is signed using nonce 3)
  //
  const configureTxHash2 = await api.configureKeys(
    sig,
    fromBalhulOwn,
    planetAddress,
    {
      encrypt: "0x5678",
      auth: "0xefab",
      cryptoSuite: 0,
      breach: false,
    }
  );
  expect(configureTxHash2).toBeTruthy();
  const txStatus6 = await api.getTransactionStatus(configureTxHash2);
  expect(txStatus6).toEqual("pending");
});

test("getRollerConfig", async () => {
  const config = await api.getRollerConfig();
  expect(config).toHaveProperty(
    "contract",
    "0x56db68f29203ff44a803faa2404a44ecbb7a7480"
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
