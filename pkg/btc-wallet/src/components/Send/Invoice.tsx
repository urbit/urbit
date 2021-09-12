import React, { useState, useEffect } from 'react';
import {
  Box,
  Icon,
  StatelessTextInput as Input,
  Row,
  Text,
  Button,
  Col,
  LoadingSpinner,
} from '@tlon/indigo-react';
import * as kg from 'urbit-key-generation';
import { patp2dec, isValidPatq, isValidPatp } from 'urbit-ob';
import * as bitcoin from 'bitcoinjs-lib';
import Sigil from '../Sigil';
import Sent from './Sent';
import { satsToCurrency } from '../../lib/util';
import Error from '../Error';
import { useSettings } from '../../hooks/useSettings';
import { api } from '../../lib/api';
import { UrbitWallet } from '../../types';

const BITCOIN_MAINNET_INFO = {
  messagePrefix: '\x18Bitcoin Signed Message:\n',
  bech32: 'bc',
  bip32: {
    public: 0x04b24746,
    private: 0x04b2430c,
  },
  pubKeyHash: 0x00,
  scriptHash: 0x05,
  wif: 0x80,
};

const BITCOIN_TESTNET_INFO = {
  messagePrefix: '\x18Bitcoin Signed Message:\n',
  bech32: 'tb',
  bip32: {
    public: 0x045f1cf6,
    private: 0x045f18bc,
  },
  pubKeyHash: 0x6f,
  scriptHash: 0xc4,
  wif: 0xef,
};

type Props = {
  stopSending: () => void;
  payee: string;
  satsAmount: number;
};

const Invoice: React.FC<Props> = ({ stopSending, payee, satsAmount }) => {
  const {
    error,
    currencyRates,
    psbt,
    fee,
    broadcastSuccess,
    network,
    denomination,
    history,
  } = useSettings();
  const [masterTicket, setMasterTicket] = useState('');
  const [ready, setReady] = useState(false);
  const [historyLength, setHistoryLength] = useState(0);
  const [localError, setLocalError] = useState(error);
  const [broadcasting, setBroadcasting] = useState(false);

  useEffect(() => {
    if (broadcasting && localError !== '') {
      setBroadcasting(false);
    }
  }, [error, broadcasting, setBroadcasting]);

  const broadCastTx = (psbtHex: string) => {
    let command = {
      'broadcast-tx': psbtHex,
    };
    return api.btcWalletCommand(command);
  };

  useEffect(() => {
    if (historyLength === 0) {
      setHistoryLength(history.length);
    }
    if (broadcasting && history.length > historyLength) {
      setBroadcasting(false);
      stopSending();
    }
  }, [history]);

  const sendBitcoin = (ticket: string, psbt: string) => {
    const newPsbt = bitcoin.Psbt.fromBase64(psbt);
    setBroadcasting(true);
    kg.generateWallet({
      ticket,
      ship: parseInt(patp2dec('~' + (window as any).ship)),
    }).then((urbitWallet: UrbitWallet) => {
      // this wasn't being used, not clear why it was pulled out.
      // const { xpub } =
      // network === 'testnet'
      // ? urbitWallet.bitcoinTestnet.keys
      // : urbitWallet.bitcoinMainnet.keys;

      const { xprv: zprv } = urbitWallet.bitcoinMainnet.keys;
      const { xprv: vprv } = urbitWallet.bitcoinTestnet.keys;

      const isTestnet = network === 'testnet';
      const derivationPrefix = isTestnet ? "m/84'/1'/0'/" : "m/84'/0'/0'/";

      const btcWallet = isTestnet
        ? bitcoin.bip32.fromBase58(vprv, BITCOIN_TESTNET_INFO)
        : bitcoin.bip32.fromBase58(zprv, BITCOIN_MAINNET_INFO);

      try {
        const hex = newPsbt.data.inputs
          .reduce((psbt, input, idx) => {
            //  removing already derived part, eg m/84'/0'/0'/0/0 becomes 0/0
            const path = input.bip32Derivation[0].path
              .split(derivationPrefix)
              .join('');
            const prv = btcWallet.derivePath(path).privateKey;
            return psbt.signInput(idx, bitcoin.ECPair.fromPrivateKey(prv));
          }, newPsbt)
          .finalizeAllInputs()
          .extractTransaction()
          .toHex();

        broadCastTx(hex);
      } catch (e) {
        setLocalError('invalid-master-ticket');
        setBroadcasting(false);
      }
    });
  };

  const checkTicket = ({
    target: { value },
  }: React.ChangeEvent<HTMLInputElement>) => {
    // TODO: port over bridge ticket validation logic
    setMasterTicket(value);
    setReady(isValidPatq(value));
    setLocalError(isValidPatq(value) ? '' : 'invalid-master-ticket');
  };

  let inputColor = 'black';
  let inputBg = 'white';
  let inputBorder = 'lightGray';

  if (error !== '') {
    inputColor = 'red';
    inputBg = 'veryLightRed';
    inputBorder = 'red';
  }

  const isShip = isValidPatp(payee);

  const icon = isShip ? (
    <Sigil ship={payee} size={24} color="black" classes={''} icon padding={5} />
  ) : (
    <Box
      backgroundColor="lighterGray"
      width="24px"
      height="24px"
      textAlign="center"
      alignItems="center"
      borderRadius="2px"
      p={1}
    >
      <Icon icon="Bitcoin" color="gray" />
    </Box>
  );

  return (
    <>
      {broadcastSuccess ? (
        <Sent payee={payee} stopSending={stopSending} satsAmount={satsAmount} />
      ) : (
        <Col
          width="100%"
          backgroundColor="white"
          borderRadius="48px"
          mb={5}
          p={5}
        >
          <Row flexDirection="row-reverse">
            <Icon icon="X" cursor="pointer" onClick={() => stopSending()} />
          </Row>
          <Col
            p={5}
            mt={4}
            backgroundColor="veryLightGreen"
            borderRadius="24px"
            alignItems="center"
          >
            <Row>
              <Text color="green" fontSize="40px">
                {satsToCurrency(satsAmount, denomination, currencyRates)}
              </Text>
            </Row>
            <Row>
              <Text
                fontWeight="bold"
                fontSize="16px"
                color="midGreen"
              >{`${satsAmount} sats`}</Text>
            </Row>
            <Row mt={2}>
              <Text fontSize="14px" color="midGreen">{`Fee: ${satsToCurrency(
                fee,
                denomination,
                currencyRates
              )} (${fee} sats)`}</Text>
            </Row>
            <Row mt={4}>
              <Text fontSize="16px" fontWeight="bold" color="gray">
                You are paying
              </Text>
            </Row>
            <Row mt={2} alignItems="center">
              {icon}
              <Text
                ml={2}
                mono
                color="gray"
                fontSize="14px"
                style={{ display: 'block', overflowWrap: 'anywhere' }}
              >
                {payee}
              </Text>
            </Row>
          </Col>
          <Row mt={3} mb={2} alignItems="center">
            <Text gray fontSize={1} fontWeight="600" mr={4}>
              Ticket
            </Text>
            <Input
              value={masterTicket}
              fontSize="14px"
              type="password"
              name="masterTicket"
              obscure={(value: string) => value.replace(/[^~-]+/g, '••••••')}
              placeholder="••••••-••••••-••••••-••••••"
              autoCapitalize="none"
              autoCorrect="off"
              color={inputColor}
              backgroundColor={inputBg}
              borderColor={inputBorder}
              onChange={(e: React.ChangeEvent<HTMLInputElement>) =>
                checkTicket(e)
              }
            />
          </Row>
          {error !== '' && (
            <Row>
              <Error fontSize="14px" error={error} />
            </Row>
          )}
          <Row flexDirection="row-reverse" mt={4} alignItems="center">
            <Button
              primary
              mr={3}
              fontSize={1}
              border="none"
              borderRadius="24px"
              color={ready && !error && !broadcasting ? 'white' : 'lighterGray'}
              backgroundColor={
                ready && !error && !broadcasting ? 'green' : 'veryLightGray'
              }
              height="48px"
              onClick={() => sendBitcoin(masterTicket, psbt)}
              disabled={!ready || error || broadcasting}
              style={{
                cursor:
                  ready && !error && !broadcasting ? 'pointer' : 'default',
              }}
            >
              Send BTC
            </Button>
            {
              // @ts-ignore
              broadcasting ? <LoadingSpinner mr={3} /> : null
            }
          </Row>
        </Col>
      )}
    </>
  );
};

export default Invoice;
