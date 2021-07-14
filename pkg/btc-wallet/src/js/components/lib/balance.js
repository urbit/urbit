import React, { useState } from 'react';
import { Row, Text, Button, Col } from '@tlon/indigo-react';
import Send from './send.js';
import CurrencyPicker from './currencyPicker.js';
import { satsToCurrency } from '../../lib/util.js';
import { useSettings } from '../../hooks/useSettings.js';
import { api } from '../../api';

const Balance = () => {
  const {
    address,
    confirmedBalance: sats,
    unconfirmedBalance: unconfirmedSats,
    denomination,
    currencyRates,
    setPsbt,
    setFee,
    setError,
    scanProgress,
  } = useSettings();
  const [sending, setSending] = useState(false);
  const [copiedButton, setCopiedButton] = useState(false);
  const [copiedString, setCopiedString] = useState(false);
  const scanning = scanProgress?.main !== null || scanProgress?.change !== null;

  const copyAddress = (arg) => {
    navigator.clipboard.writeText(address);
    api.btcWalletCommand({ 'gen-new-address': null });

    if (arg === 'button') {
      setCopiedButton(true);
      setTimeout(() => {
        setCopiedButton(false);
      }, 2000);
    } else if (arg === 'string') {
      setCopiedString(true);
      setTimeout(() => {
        setCopiedString(false);
      }, 2000);
    }
  };

  const unconfirmedString = unconfirmedSats ? ` (${unconfirmedSats}) ` : '';

  const value = satsToCurrency(sats, denomination, currencyRates);
  const sendDisabled = sats === 0;
  const addressText =
    address === null ? '' : address.slice(0, 6) + '...' + address.slice(-6);

  const conversion = currencyRates[denomination]?.last;

  return (
    <>
      {sending ? (
        <Send
          value={value}
          conversion={conversion}
          stopSending={() => {
            setSending(false);
            setPsbt('');
            setFee(0);
            setError('');
          }}
        />
      ) : (
        <Col
          height="400px"
          width="100%"
          backgroundColor="white"
          borderRadius="48px"
          justifyContent="space-between"
          mb={5}
          p={5}
        >
          <Row justifyContent="space-between">
            <Text color="orange" fontSize={1}>
              Balance
            </Text>
            <Text
              color="lightGray"
              fontSize="14px"
              mono
              style={{ cursor: 'pointer' }}
              onClick={() => copyAddress('string')}
            >
              {copiedString ? 'copied' : addressText}
            </Text>
            <CurrencyPicker />
          </Row>
          <Col justifyContent="center" alignItems="center">
            <Text
              fontSize="40px"
              color="orange"
              style={{ whiteSpace: 'nowrap' }}
            >
              {value}
            </Text>
            {scanning ? (
              <Col alignItems="center">
                <Row>
                  <Text fontSize={1} color="orange">
                    Balance will be updated shortly:
                  </Text>
                </Row>
                <Row>
                  <Text fontSize={1} color="orange">
                    {scanProgress.main} main wallet addresses scanned
                    {scanProgress.change} change wallet addresses scanned
                  </Text>
                </Row>
              </Col>
            ) : (
              <Text
                fontSize={1}
                color="orange"
              >{`${sats}${unconfirmedString} sats`}</Text>
            )}
          </Col>
          <Row flexDirection="row-reverse">
            <Button
              disabled={sendDisabled}
              fontSize={1}
              fontWeight="bold"
              color={sendDisabled ? 'lighterGray' : 'white'}
              backgroundColor={sendDisabled ? 'veryLightGray' : 'orange'}
              style={{ cursor: sendDisabled ? 'default' : 'pointer' }}
              borderColor="none"
              borderRadius="24px"
              height="48px"
              onClick={() => setSending(true)}
            >
              Send
            </Button>
            <Button
              mr={3}
              disabled={copiedButton}
              fontSize={1}
              fontWeight="bold"
              color={copiedButton ? 'green' : 'orange'}
              backgroundColor={copiedButton ? 'veryLightGreen' : 'midOrange'}
              style={{
                cursor: copiedButton ? 'default' : 'pointer',
              }}
              borderColor="none"
              borderRadius="24px"
              height="48px"
              onClick={() => copyAddress('button')}
            >
              {copiedButton ? 'Address Copied!' : 'Copy Address'}
            </Button>
          </Row>
        </Col>
      )}
    </>
  );
};

export default Balance;
