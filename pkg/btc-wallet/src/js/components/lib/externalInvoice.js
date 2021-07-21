import React, { useEffect, useRef, useState } from 'react';
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
import { Sigil } from './sigil.js';
import * as bitcoin from 'bitcoinjs-lib';
import { isValidPatp } from 'urbit-ob';
import Sent from './sent.js';
import Error from './error.js';
import { copyToClipboard, satsToCurrency } from '../../lib/util.js';
import { useSettings } from '../../hooks/useSettings.js';
import { api } from '../../api';

const ExternalInvoice = ({ payee, stopSending, satsAmount }) => {
  const { error, currencyRates, fee, broadcastSuccess, denomination, psbt } =
    useSettings();
  const [txHex, setTxHex] = useState('');
  const [ready, setReady] = useState(false);
  const [copiedPsbt, setCopiedPsbt] = useState(false);
  const [downloadedButton, setDownloadedButton] = useState(false);
  const [copiedButton, setCopiedButton] = useState(false);
  const [localError, setLocalError] = useState('');
  const [broadcasting, setBroadcasting] = useState(false);
  const invoiceRef = useRef();

  useEffect(() => {
    if (broadcasting && localError !== '') {
      setBroadcasting(false);
    }
    if (error !== '') {
      setLocalError(error);
    }
  }, [error, broadcasting, setBroadcasting]);

  const broadCastTx = (hex) => {
    let command = {
      'broadcast-tx': hex,
    };
    return api.btcWalletCommand(command);
  };

  const sendBitcoin = (hex) => {
    try {
      bitcoin.Transaction.fromHex(hex);
      broadCastTx(hex);
      setBroadcasting(true);
    } catch (e) {
      setLocalError('invalid-signed');
      setBroadcasting(false);
    }
  };

  const checkTxHex = (e) => {
    setTxHex(e.target.value);
    setReady(txHex.length > 0);
    setLocalError('');
  };

  const copyPsbt = () => {
    copyToClipboard(psbt);
    setCopiedPsbt(true);
    setCopiedButton(true);
    setTimeout(() => {
      setCopiedPsbt(false);
      setCopiedButton(false);
    }, 2000);
  };

  const downloadPsbtFile = () => {
    setDownloadedButton(true);
    const blob = new Blob([psbt]);
    const downloadURL = URL.createObjectURL(blob);
    const link = document.createElement('a');
    link.href = downloadURL;
    link.setAttribute('download', 'urbit.psbt');
    document.body.appendChild(link);
    link.click();
    link.parentNode.removeChild(link);
    setTimeout(() => {
      setDownloadedButton(false);
    }, 1000);
  };

  let inputColor = 'black';
  let inputBg = 'white';
  let inputBorder = 'lightGray';

  if (localError !== '') {
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
          ref={invoiceRef}
          width="100%"
          backgroundColor="white"
          borderRadius="48px"
          mb={5}
          p={5}
        >
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
          <Box mt={3}>
            <Text fontSize="14px" fontWeight="500">
              Partially-signed Bitcoin Transaction (PSBT)
            </Text>
          </Box>
          <Box mt={3}>
            <Text
              mono
              color="lightGray"
              fontSize="14px"
              style={{ overflowWrap: 'anywhere', cursor: 'pointer' }}
              onClick={() => copyPsbt()}
            >
              {copiedPsbt ? 'copied' : psbt}
            </Text>
          </Box>
          <Box mt={3} mb={2}>
            <Text gray fontSize="14px">
              Paste the signed transaction from your external wallet:
            </Text>
          </Box>
          <Input
            value={txHex}
            fontSize="14px"
            placeholder="..."
            autoCapitalize="none"
            autoCorrect="off"
            color={inputColor}
            backgroundColor={inputBg}
            borderColor={inputBorder}
            style={{ lineHeight: '4' }}
            onChange={(e) => checkTxHex(e)}
          />
          {localError !== '' && (
            <Row>
              <Error error={localError} fontSize="14px" mt={2} />
            </Row>
          )}
          <Row
            flexDirection="row"
            mt={4}
            alignItems="center"
            justifyContent="center"
          >
            <Button
              mr={3}
              disabled={downloadedButton}
              fontSize={1}
              fontWeight="bold"
              color={downloadedButton ? 'green' : 'orange'}
              backgroundColor={
                downloadedButton ? 'veryLightGreen' : 'midOrange'
              }
              style={{
                cursor: downloadedButton ? 'default' : 'pointer',
              }}
              borderColor="none"
              borderRadius="24px"
              height="48px"
              onClick={() => downloadPsbtFile()}
            >
              {downloadedButton ? 'PSBT Downloading' : 'Download PSBT'}
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
              onClick={() => copyPsbt()}
            >
              {copiedButton ? 'PSBT Copied!' : 'Copy PSBT'}
            </Button>
            <Button
              primary
              mr={3}
              fontSize={1}
              borderRadius="24px"
              border="none"
              height="48px"
              onClick={() => sendBitcoin(txHex)}
              disabled={!ready || localError || broadcasting}
              color={
                ready && !localError && !broadcasting ? 'white' : 'lighterGray'
              }
              backgroundColor={
                ready && !localError && !broadcasting
                  ? 'green'
                  : 'veryLightGray'
              }
              style={{
                cursor: ready && !localError ? 'pointer' : 'default',
              }}
            >
              Send BTC
            </Button>
            {broadcasting ? <LoadingSpinner mr={3} /> : null}
          </Row>
        </Col>
      )}
    </>
  );
};

export default ExternalInvoice;
