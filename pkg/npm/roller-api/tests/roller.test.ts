import RollerRPCAPI, {
  L2Point,
  EthAddress,
  Hash,
  Ship,
} from "../client/typescript/src/index";

const api = new RollerRPCAPI({
  transport: {
    type: "http",
    host: "localhost",
    port: 8080,
    path: "/v1/roller",
  },
});

// TODO: add tests for all RPC methods

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

test("spawn", async () => {
  const sig = "0x1234", // We fake the signing in the backend
    from = {
      ship: "~wanzod",
      proxy: "own",
    },
    data = {
      address: "0xf48062Ae8BAfD6Ef19CD6cb89db93A0d0ca6ce2",
      ship: "~norsyr-torryn",
    };
  const address: EthAddress = "0x6deffb0cafdb11d175f123f6891aa64f01c24f7d";
  const hash: Hash = await api.spawn(sig, from, address, data);
  expect(typeof hash).toBe("string");
});
