const BITCOIN_NETWORK_TYPES = {
  LOCAL: Symbol('LOCAL'),
  TESNET: Symbol('TESNET'),
  REGTEST: Symbol('REGTEST'),
  MAINNET: Symbol('MAINNET'),
};

const renderNetworkType = network =>
  network === BITCOIN_NETWORK_TYPES.TESTNET
    ? 'testnet'
    : network === BITCOIN_NETWORK_TYPES.REGTEST
    ? 'regtest'
    : network === BITCOIN_NETWORK_TYPES.MAINNET
    ? 'main'
    : network === BITCOIN_NETWORK_TYPES.LOCAL
    ? 'Local Node'
    : 'Offline';

export { BITCOIN_NETWORK_TYPES, renderNetworkType };
