import React, { createContext, useContext, useEffect, useState } from 'react';
import _ from 'lodash';
import { api } from '../api';
import { reduceHistory } from '../lib/util';

export const SettingsContext = createContext({
  network: 'bitcoin',
  loadedBtc: false,
  setLoadedBtc: () => {},
  loadedSettings: false,
  setLoadedSettings: () => {},
  loaded: false,
  setLoaded: () => {},
  providerPerms: {},
  setProviderPerms: () => {},
  shipWallets: {},
  setShipWallets: () => {},
  provider: null,
  setProvider: () => {},
  wallet: null,
  setWallet: () => {},
  confirmedBalance: 0,
  setConfirmedBalance: () => {},
  unconfirmedBalance: 0,
  setUnconfirmedBalance: () => {},
  btcState: null,
  setBtcState: () => {},
  history: [],
  setHistory: () => {},
  psbt: '',
  setPsbt: () => {},
  address: null,
  setAddress: () => {},
  currencyRates: {
    BTC: { last: 1, symbol: 'BTC' },
  },
  denomination: 'BTC',
  showWarning: true,
  setShowWarning: () => {},
  error: '',
  setError: () => {},
  broadcastSuccess: false,
  setBroadcastSuccess: () => {},
});

export const SettingsProvider = ({ channel, children }) => {
  const [network, setNetwork] = useState('bitcoin');
  const [channelData, setChannelData] = useState(null);
  const [loadedBtc, setLoadedBtc] = useState(false);
  const [loadedSettings, setLoadedSettings] = useState(false);
  const [loaded, setLoaded] = useState(false);
  const [providerPerms, setProviderPerms] = useState({});
  const [shipWallets, setShipWallets] = useState({});
  const [provider, setProvider] = useState(null);
  const [wallet, setWallet] = useState(null);
  const [confirmedBalance, setConfirmedBalance] = useState(0);
  const [unconfirmedBalance, setUnconfirmedBalance] = useState(0);
  const [btcState, setBtcState] = useState(null);
  const [history, setHistory] = useState([]);
  const [psbt, setPsbt] = useState('');
  const [address, setAddress] = useState(null);
  const [currencyRates, setCurrencyRates] = useState({
    BTC: { last: 1, symbol: 'BTC' },
  });
  const [denomination, setDenomination] = useState('BTC');
  const [showWarning, setShowWarning] = useState(false);
  const [error, setError] = useState('');
  const [broadcastSuccess, setBroadcastSuccess] = useState(false);

  const { Provider } = SettingsContext;

  const success = (event) => {
    console.log({ event });
    setChannelData(event);
  };
  const fail = (error) => console.log({ error });

  const initializeBtcWallet = () => {
    api.bind('/all', 'PUT', api.ship, 'btc-wallet', success, fail);
  };

  const initializeSettings = () => {
    let app = 'settings-store';
    let path = '/bucket/btc-wallet';

    fetch(`/~/scry/${app}${path}.json`)
      .then((res) => res.json())
      .then((n) => {
        let data = _.get(n, 'initial', false);
        let bucketData = _.get(n, 'bucket', false);
        if (data) {
          setChannelData(n);
        }
        if (bucketData) {
          let bucketWarning = _.get(n, 'bucket.warning', -1);
          if (bucketWarning !== -1) {
            setShowWarning(bucketWarning);
          }
          let bucketCurrency = _.get(n, 'bucket.currency', -1);
          if (bucketCurrency !== -1) {
            setDenomination(bucketCurrency);
          }
          setLoadedSettings(true);
          if (loadedBtc) {
            setLoaded(true);
          }
        }
      });

    api.bind(path, 'PUT', api.ship, app, success, fail);
  };

  const initializeCurrencyPoll = () => {
    fetch('https://blockchain.info/ticker')
      .then((res) => res.json())
      .then((n) => {
        const newCurrencyRates = currencyRates;
        for (let c in n) {
          newCurrencyRates[c] = n[c];
        }
        setCurrencyRates(newCurrencyRates);
        setTimeout(() => initializeCurrencyPoll(), 1000 * 60 * 15);
      });
  };

  const start = () => {
    if (api.ship) {
      initializeBtcWallet();
      initializeSettings();
      initializeCurrencyPoll();
    }
  };

  const handleNewTx = ({ txid, recvd }) => {
    let old = _.findIndex(history, (h) => {
      return h.txid.dat === txid.dat && h.txid.wid === txid.wid;
    });
    if (old !== -1) {
      delete history.splice(old, 1);
    }
    if (recvd === null) {
      history.unshift({ txid, recvd });
    } else {
      // we expect history to have null recvd values first, and the rest in
      // descending order
      let insertionIndex = _.findIndex(history, (h) => {
        return h.recvd < recvd && h.recvd !== null;
      });
      history.splice(insertionIndex, 0, { txid, recvd });
    }
  };

  const handleCancelTx = ({ wid, dat }) => {
    let entryIndex = _.findIndex(history, (h) => {
      return wid === h.txid.wid && dat === h.txid.dat;
    });
    if (entryIndex > -1) {
      history[entryIndex].failure = true;
    }
  };

  useEffect(() => {
    const initialData = channelData?.data?.initial;
    const putEntryData = channelData?.data?.['settings-event']?.['put-entry'];
    const btcStateData = channelData?.data?.['btc-state'];
    const changeProvider = channelData?.data?.['change-provider'];
    const newTx = channelData?.data?.['new-tx'];
    const providerStatus = channelData?.data?.providerStatus;
    const checkPayee = channelData?.data?.checkPayee;
    const changeWallet = channelData?.data?.changeWallet;
    const psbtData = channelData?.data.psbt;
    const cancelTx = channelData?.data['cancel-tx'];
    const addressData = channelData?.data?.address;
    const balanceData = channelData?.data?.balance;
    const errorData = channelData?.data?.error;
    const broadcastSuccessData = channelData?.data?.['broadcast-success'];
    const broadcastFailData = channelData?.data?.['broadcast-fail'];
    if (initialData) {
      setProvider(initialData.provider);
      setWallet(initialData.wallet);
      setConfirmedBalance(_.get(initialData.balance, 'confirmed', null));
      setUnconfirmedBalance(_.get(initialData.balance, 'unconfirmed', null));
      setBtcState(initialData['btc-state']);
      setHistory(reduceHistory(initialData.history));
      setAddress(initialData.address);
      setLoadedBtc(true);
      if (loadedSettings) {
        setLoaded(true);
      }
    }
    if (putEntryData && putEntryData?.['entry-key'] === 'currency') {
      setDenomination(putEntryData.value);
    }
    if (putEntryData && putEntryData?.['entry-key'] === 'warning') {
      setShowWarning(putEntryData.value);
    }
    if (btcStateData) {
      setBtcState(btcStateData);
    }
    if (changeProvider) {
      setProvider(changeProvider);
    }
    if (newTx) {
      handleNewTx(newTx);
    }
    if (providerStatus) {
      let newProviderPerms = providerPerms;
      for (let c in providerStatus) {
        newProviderPerms[c] = providerStatus[c];
      }
      setProviderPerms(newProviderPerms);
    }
    if (checkPayee) {
      let newShipWallets = shipWallets;

      for (let c in checkPayee) {
        newShipWallets[c] = checkPayee[c];
      }
      setShipWallets(newShipWallets);
    }
    if (changeWallet) {
      setWallet(changeWallet);
    }
    if (psbtData) {
      setPsbt(psbtData.pb);
    }
    if (cancelTx) {
      handleCancelTx(cancelTx);
    }
    if (addressData) {
      setAddress(addressData);
    }
    if (balanceData) {
      setUnconfirmedBalance(balanceData.unconfirmed);
      setConfirmedBalance(balanceData.confirmed);
    }
    if (errorData) {
      setError(errorData);
    }
    if (broadcastSuccessData) {
      setBroadcastSuccess(true);
    }
    if (broadcastFailData) {
      setBroadcastSuccess(false);
    }
  }, [channelData]);

  useEffect(() => {
    channel.setOnChannelError(() => {
      start();
    });
    start();
  }, []);

  return (
    <Provider
      value={{
        network,
        setNetwork,
        loadedBtc,
        setLoadedBtc,
        loadedSettings,
        setLoadedSettings,
        loaded,
        setLoaded,
        providerPerms,
        setProviderPerms,
        shipWallets,
        setShipWallets,
        provider,
        setProvider,
        wallet,
        setWallet,
        confirmedBalance,
        setConfirmedBalance,
        unconfirmedBalance,
        setUnconfirmedBalance,
        btcState,
        setBtcState,
        history,
        setHistory,
        psbt,
        setPsbt,
        address,
        setAddress,
        currencyRates,
        setCurrencyRates,
        denomination,
        setDenomination,
        showWarning,
        setShowWarning,
        error,
        setError,
        broadcastSuccess,
        setBroadcastSuccess,
      }}
    >
      {children}
    </Provider>
  );
};

export const useSettings = () => useContext(SettingsContext);
