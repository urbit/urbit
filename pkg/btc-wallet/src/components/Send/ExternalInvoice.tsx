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
import Sigil from '../Sigil';
import * as bitcoin from 'bitcoinjs-lib';
import { isValidPatp } from 'urbit-ob';
import Sent from './Sent.tsx';
import Error from '../Error';
import { copyToClipboard, satsToCurrency } from '../../lib/util.ts';
import { useSettings } from '../../hooks/useSettings.tsx';
import { api } from '../../lib/api';

type Props = {
  payee: string;
  stopSending: () => void;
  satsAmount: number;
};

const ExternalInvoice: React.FC<Props> = ({
  payee,
  stopSending,
  satsAmount,
}) => {
  const { error, currencyRates, fee, broadcastSuccess, denomination, psbt } =
    useSettings();
  const [txHex, setTxHex] = useState('');
  const [ready, setReady] = useState(false);
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

  const broadCastTx = (hex: string) => {
    let command = {
      'broadcast-tx': hex,
    };
    return api.btcWalletCommand(command);
  };

  const sendBitcoin = (hex: string) => {
    try {
      bitcoin.Transaction.fromHex(hex);
      broadCastTx(hex);
      setBroadcasting(true);
    } catch (e) {
      setLocalError('invalid-signed');
      setBroadcasting(false);
    }
  };

  const checkTxHex = (e: React.ChangeEvent<HTMLInputElement>) => {
    setTxHex(e.target.value);
    setReady(txHex.length > 0);
    setLocalError('');
  };

  const copyPsbt = () => {
    copyToClipboard(psbt);
  };

  const downloadPsbtFile = () => {
    const blob = new Blob([psbt]);
    const downloadURL = URL.createObjectURL(blob);
    const link = document.createElement('a');
    link.href = downloadURL;
    link.setAttribute('download', 'urbit.psbt');
    document.body.appendChild(link);
    link.click();
    link.parentNode.removeChild(link);
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
            <Row flexDirection="row">
              <Button
                borderRadius="24px"
                border="none"
                width="24px"
                height="24px"
                padding="0px"
                backgroundColor="veryLightGray"
                onClick={() => downloadPsbtFile()}
                mr={2}
              >
                <Icon icon="Download" width="12px" />
              </Button>
              <Button
                borderRadius="24px"
                border="none"
                width="24px"
                height="24px"
                padding="0px"
                backgroundColor="veryLightGray"
                onClick={() => copyPsbt()}
              >
                <Icon icon="Copy" />
              </Button>
            </Row>
          </Box>
          <Row
            justifyContent="flex-end"
            alignItems="center"
            flexDirection="row"
          >
            <Text gray bold fontSize="16px" mr={2}>
              Signed Tx
            </Text>
            <Input
              value={txHex}
              fontSize="14px"
              placeholder="7f3..."
              autoCapitalize="none"
              autoCorrect="off"
              color={inputColor}
              width="70%"
              backgroundColor={inputBg}
              borderColor={inputBorder}
              style={{ lineHeight: '4' }}
              onChange={(e: React.ChangeEvent<HTMLInputElement>) =>
                checkTxHex(e)
              }
            />
            {localError !== '' && (
              <Row>
                <Error error={localError} fontSize="14px" />
              </Row>
            )}
          </Row>
          <Row
            flexDirection="row"
            mt={4}
            alignItems="center"
            justifyContent="center"
          >
            <Button
              primary
              mr={3}
              fontSize={1}
              borderRadius="24px"
              border="none"
              height="48px"
              width="100%"
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
            {broadcasting ? <LoadingSpinner /> : null}
          </Row>
        </Col>
      )}
    </>
  );
};

export default ExternalInvoice;
