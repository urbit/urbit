export type ProviderPerms = {
  provider: string;
  permitted: boolean;
};

export type ShipWallets = {
  payee: string;
  hasWallet: boolean;
};

export type Transaction = {
  txid: TxidType;
  recvd: number;
  outputs: [{ ship: string; val: { value: number } }];
  inputs: [{ ship: string }];
  failure: string;
};

export type TxidType = {
  dat: string;
  wid: string;
};

export type ScanProgress = {
  main: null | number;
  change: null | number;
};

export type Network = 'bitcoin' | 'testnet';

export type Denomination = 'BTC' | 'USD';

export type UrbitWallet = {
  bitcoinTestnet: { keys: { xpub: string; xprv: string } };
  bitcoinMainnet: { keys: { xpub: string; xprv: string } };
};

export type CurrencyRate = {
  [Denomination: string]: { last: number; symbol: string };
};

export type Provider = {
  host: string;
  connected: boolean;
};
