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
  planetMGMTAddress: EthAddress = "0x218f6f87683db546ad47a5dc8b480e5a9b694866",
  sig: Signature = "0x1234", // We fake the signing in the backend
  fromStar: From = {
    ship: "~wanzod",
    proxy: "own",
  },
  fromPlanetTransfer: From = {
    ship: "~balhul-polsub",
    proxy: "transfer",
  },
  fromPlanetOwn: From = {
    ship: "~balhul-polsub",
    proxy: "own",
  };

// L2 Invite Flow - A -
//
//   The sponsor star configures keys and sets the planet's management proxy
//
test("inviteFlowA", async () => {
  // ~wanzod spawn ~balhul-polsub
  //
  const spawnTxHash = await api.spawn(sig, fromStar, starAddress, {
    address: spawnAddress,
    ship: "~balhul-polsub",
  });
  expect(spawnTxHash).toBeTruthy();
  const txStatus1 = await api.getTransactionStatus(spawnTxHash);
  expect(txStatus1).toEqual("pending");

  // ...configures keys
  //
  const configureTxHash1 = await api.configureKeys(sig, fromStar, starAddress, {
    encrypt: "0x1234",
    auth: "0xabcd",
    cryptoSuite: 0,
    breach: false,
  });
  expect(configureTxHash1).toBeTruthy();
  const txStatus2 = await api.getTransactionStatus(configureTxHash1);
  expect(txStatus2).toEqual("pending");

  // ...and sets the management proxy so it's ready to boot
  //
  const mgmtTxHash1 = await api.setManagementProxy(sig, fromStar, starAddress, {
    address: planetMGMTAddress,
  });
  expect(mgmtTxHash1).toBeTruthy();
  const txStatus3 = await api.getTransactionStatus(mgmtTxHash1);
  expect(txStatus3).toEqual("pending");

  // When the Roller submits the transactions up to this point to an Ethereum node, and
  // they get confirmed on L1, ~balhul-polsub is ready to boot inmediately, but ~wanzod
  // still controls the planet (the ownership proxy of ~balhul-polsub is set to ~wanzod's
  // ownership proxy)
  //
  // In order for ~balhul-polsub to gain full control/ownership the following is needed

  // ~balhul-polsub accepts the transfer, resetting all proxies
  //
  const transferTxHash = await api.transferPoint(
    sig,
    fromPlanetTransfer,
    planetAddress,
    { address: planetAddress, reset: true }
  );
  expect(transferTxHash).toBeTruthy();
  const txStatus4 = await api.getTransactionStatus(transferTxHash);
  expect(txStatus4).toEqual("pending");

  // ...configures keys
  //
  const configureTxHash2 = await api.configureKeys(sig, fromStar, starAddress, {
    encrypt: "0x5678",
    auth: "0xefab",
    cryptoSuite: 0,
    breach: false,
  });
  expect(configureTxHash2).toBeTruthy();
  const txStatus5 = await api.getTransactionStatus(configureTxHash2);
  expect(txStatus5).toEqual("pending");

  // ...ans sets the management proxy so it's ready to boot
  //
  const mgmtTxHash2 = await api.setManagementProxy(sig, fromStar, starAddress, {
    address: planetMGMTAddress,
  });
  expect(mgmtTxHash2).toBeTruthy();
  const txStatus6 = await api.getTransactionStatus(mgmtTxHash2);
  expect(txStatus6).toEqual("pending");
});

// L2 Invite Flow - B -
//
//   The spawned planet configures keys and sets its management proxy
//
test("inviteFlowB", async () => {
  // setSpawnProxy for ~wanzod
  //
  const proxyTxHash = await api.setSpawnProxy(sig, fromStar, starAddress, {
    address: starAddress,
  });
  expect(proxyTxHash).toBeTruthy();
  const txStatus1 = await api.getTransactionStatus(proxyTxHash);
  expect(txStatus1).toEqual("pending");

  // spawn ~balhul-polsub
  //
  const spawnTxHash = await api.spawn(sig, fromStar, starAddress, {
    address: spawnAddress,
    ship: "~balhul-polsub",
  });
  expect(spawnTxHash).toBeTruthy();
  const txStatus2 = await api.getTransactionStatus(spawnTxHash);
  expect(txStatus2).toEqual("pending");

  // ~balhul-polsub accepts the transfer...
  //
  const transferTxHash = await api.transferPoint(
    sig,
    fromPlanetTransfer,
    planetAddress,
    { address: planetAddress }
  );
  expect(transferTxHash).toBeTruthy();
  const txStatus3 = await api.getTransactionStatus(transferTxHash);
  expect(txStatus3).toEqual("pending");

  // ...configures keys
  //
  const configureTxHash = await api.configureKeys(
    sig,
    fromPlanetOwn,
    planetAddress,
    { encrypt: "0x1234", auth: "0xabcd", cryptoSuite: 0, breach: false }
  );
  expect(configureTxHash).toBeTruthy();
  const txStatus4 = await api.getTransactionStatus(transferTxHash);
  expect(txStatus4).toEqual("pending");

  // ...and sets the management proxy so it's ready to boot
  //
  const mgmtTxHash = await api.setManagementProxy(
    sig,
    fromPlanetOwn,
    planetAddress,
    { address: planetMGMTAddress }
  );
  expect(mgmtTxHash).toBeTruthy();
  const txStatus5 = await api.getTransactionStatus(transferTxHash);
  expect(txStatus5).toEqual("pending");
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
