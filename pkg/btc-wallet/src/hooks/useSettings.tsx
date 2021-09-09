import React, { createContext, useContext, useEffect, useState } from 'react';
import _ from 'lodash';
import { api } from '../lib/api';
import { mapDenominationToSymbol, reduceHistory } from '../lib/util';
import {
  CurrencyRate,
  Denomination,
  Network,
  Provider,
  ProviderPerms,
  ScanProgress,
  ShipWallets,
  Transaction,
  TxidType,
} from '../types';

type SettingsContextType = {
  network: Network;
  setNetwork: React.Dispatch<React.SetStateAction<Network>>;
  loadedBtc: boolean;
  setLoadedBtc: React.Dispatch<React.SetStateAction<boolean>>;
  loadedSettings: boolean;
  setLoadedSettings: React.Dispatch<React.SetStateAction<boolean>>;
  loaded: boolean;
  setLoaded: React.Dispatch<React.SetStateAction<boolean>>;
  providerPerms: ProviderPerms;
  setProviderPerms: React.Dispatch<React.SetStateAction<ProviderPerms>>;
  shipWallets: ShipWallets;
  setShipWallets: React.Dispatch<React.SetStateAction<ShipWallets>>;
  provider: Provider;
  setProvider: React.Dispatch<React.SetStateAction<string | null>>;
  wallet: string | null;
  setWallet: React.Dispatch<React.SetStateAction<string | null>>;
  confirmedBalance: number;
  setConfirmedBalance: React.Dispatch<React.SetStateAction<number>>;
  unconfirmedBalance: number;
  setUnconfirmedBalance: React.Dispatch<React.SetStateAction<number>>;
  btcState: any;
  setBtcState: React.Dispatch<React.SetStateAction<any>>;
  history: Transaction[];
  setHistory: React.Dispatch<React.SetStateAction<Transaction[]>>;
  fee: number;
  setFee: React.Dispatch<React.SetStateAction<number>>;
  psbt: string;
  setPsbt: React.Dispatch<React.SetStateAction<string>>;
  address: string | null;
  setAddress: React.Dispatch<React.SetStateAction<string | null>>;
  currencyRates: CurrencyRate;
  setCurrencyRates: React.Dispatch<React.SetStateAction<{}>>;
  denomination: Denomination;
  setDenomination: React.Dispatch<React.SetStateAction<Denomination>>;
  showWarning: boolean;
  setShowWarning: React.Dispatch<React.SetStateAction<boolean>>;
  error: string;
  setError: React.Dispatch<React.SetStateAction<string>>;
  broadcastSuccess: boolean;
  setBroadcastSuccess: React.Dispatch<React.SetStateAction<boolean>>;
  scanProgress: ScanProgress;
  setScanProgress: React.Dispatch<React.SetStateAction<ScanProgress>>;
};

export const SettingsContext = createContext<SettingsContextType>({
  network: 'bitcoin',
  setNetwork: () => {},
  loadedBtc: false,
  setLoadedBtc: () => {},
  loadedSettings: false,
  setLoadedSettings: () => {},
  loaded: false,
  setLoaded: () => {},
  providerPerms: { provider: '', permitted: false },
  setProviderPerms: () => {},
  shipWallets: { payee: '', hasWallet: false },
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
  fee: 0,
  setFee: () => {},
  psbt: '',
  setPsbt: () => {},
  address: null,
  setAddress: () => {},
  currencyRates: {
    BTC: { last: 1, symbol: 'BTC' },
  },
  setCurrencyRates: () => {},
  denomination: 'BTC',
  setDenomination: () => {},
  showWarning: true,
  setShowWarning: () => {},
  error: '',
  setError: () => {},
  broadcastSuccess: false,
  setBroadcastSuccess: () => {},
  scanProgress: { main: null, change: null },
  setScanProgress: () => {},
});

type Props = {
  channel: { setOnChannelError: (arg: () => void) => void };
};

export const SettingsProvider: React.FC<Props> = ({ channel, children }) => {
  const [network, setNetwork] = useState<Network>('bitcoin');
  const [channelData, setChannelData] = useState(null);
  const [loadedBtc, setLoadedBtc] = useState(false);
  const [loadedSettings, setLoadedSettings] = useState(false);
  const [loaded, setLoaded] = useState(false);
  const [providerPerms, setProviderPerms] = useState<ProviderPerms>({
    provider: '',
    permitted: false,
  });
  const [shipWallets, setShipWallets] = useState<ShipWallets>({
    payee: '',
    hasWallet: false,
  });
  const [provider, setProvider] = useState(null);
  const [wallet, setWallet] = useState(null);
  const [confirmedBalance, setConfirmedBalance] = useState(0);
  const [unconfirmedBalance, setUnconfirmedBalance] = useState(0);
  const [btcState, setBtcState] = useState(null);
  const [history, setHistory] = useState([]);
  const [psbt, setPsbt] = useState('');
  const [fee, setFee] = useState(0);
  const [address, setAddress] = useState(null);
  const [currencyRates, setCurrencyRates] = useState({
    BTC: { last: 1, symbol: 'BTC' },
  });
  const [denomination, setDenomination] = useState<Denomination>('BTC');
  const [showWarning, setShowWarning] = useState(false);
  const [error, setError] = useState('');
  const [broadcastSuccess, setBroadcastSuccess] = useState(false);
  const [scanProgress, setScanProgress] = useState({
    main: null,
    change: null,
  });

  const { Provider } = SettingsContext;

  const success = (event: any) => {
    console.log({ event });
    setChannelData(event);
  };
  const fail = (error: any) => console.log({ error });

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
        const newCurrencyRates: any = currencyRates;
        for (let c in n) {
          newCurrencyRates[c] = n[c];
          newCurrencyRates[c].symbol = mapDenominationToSymbol(c);
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

  const handleNewTx = (newTx: Transaction) => {
    const { txid, recvd } = newTx;
    let old = _.findIndex(history, (h: Transaction) => {
      return h.txid.dat === txid.dat && h.txid.wid === txid.wid;
    });
    if (old !== -1) {
      const newHistory = history.filter((_, i) => i !== old);
      setHistory(newHistory);
    }
    if (recvd === null && old === -1) {
      const newHistory = [...history, newTx];
      setHistory(newHistory);
    } else if (recvd !== null && old === -1) {
      // we expect history to have null recvd values first, and the rest in
      // descending order
      let insertionIndex = _.findIndex(history, (h: Transaction) => {
        return h.recvd < recvd && h.recvd !== null;
      });
      const newHistory = history.map((o, i) =>
        i === insertionIndex ? newTx : o
      );
      setHistory(newHistory);
    }
  };

  const handleCancelTx = ({ wid, dat }: TxidType) => {
    let entryIndex = _.findIndex(history, (h: Transaction) => {
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
    const scanProgressData = channelData?.data?.['scan-progress'];
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
      let newProviderPerms: any = providerPerms;
      for (let c in providerStatus) {
        newProviderPerms[c] = providerStatus[c];
      }
      setProviderPerms(newProviderPerms);
    }
    if (checkPayee) {
      let newShipWallets: any = shipWallets;

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
      setFee(psbtData.fee);
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
    if (scanProgressData) {
      setScanProgress(scanProgressData);
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
        fee,
        setFee,
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
        scanProgress,
        setScanProgress,
      }}
    >
      {children}
    </Provider>
  );
};

export const useSettings = () => useContext(SettingsContext);
