import React, { useEffect, useState } from 'react';
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
import Invoice from './Invoice';
import BridgeInvoice from './BridgeInvoice';
import ExternalInvoice from './ExternalInvoice';
import FeePicker from './FeePicker';
import Error from '../Error';
import Signer from './Signer';
import { validate } from 'bitcoin-address-validation';
import * as ob from 'urbit-ob';
import { useSettings } from '../../hooks/useSettings';
import { api } from '../../lib/api';
import { deSig } from '../../lib/util';

enum focusFields {
  payee,
  currency,
  sats,
  note,
  empty = '',
}

export enum feeLevels {
  low,
  mid,
  high,
}

export enum signMethods {
  bridge = 'bridge',
  masterTicket = 'masterTicket',
  external = 'external',
}

enum payeeTypes {
  ship,
  address,
  initial = '',
}

export type FeeChoices = {
  [feeLevels.low]: [number, number];
  [feeLevels.mid]: [number, number];
  [feeLevels.high]: [number, number];
};

type Props = {
  stopSending: () => void;
  value: string;
  conversion: number;
};

const Send: React.FC<Props> = ({ stopSending, value, conversion }) => {
  const { error, setError, network, psbt, denomination, shipWallets } =
    useSettings();
  const [signing, setSigning] = useState(false);
  const [denomAmount, setDenomAmount] = useState(0.0);
  const [satsAmount, setSatsAmount] = useState(0);
  const [payee, setPayee] = useState('');
  const [checkingPatp, setCheckingPatp] = useState(false);
  const [payeeType, setPayeeType] = useState<payeeTypes>(payeeTypes.initial);
  const [ready, setReady] = useState(false);
  const [validPayee, setValidPayee] = useState(false);
  const [focusedField, setFocusedField] = useState(focusFields.empty);
  const [feeChoices, setFeeChoices] = useState<FeeChoices>({
    [feeLevels.low]: [10, 1],
    [feeLevels.mid]: [10, 1],
    [feeLevels.high]: [10, 1],
  });
  const [feeValue, setFeeValue] = useState(feeLevels.mid);
  const [showFeePicker, setShowFeePicker] = useState(false);
  const [note, setNote] = useState('');
  const [choosingSignMethod, setChoosingSignMethod] = useState(false);
  const [signMethod, setSignMethod] = useState<signMethods>(signMethods.bridge);

  const feeDismiss = () => {
    setShowFeePicker(false);
  };

  const handleSetSignMethod = (signMethod: signMethods) => {
    setSignMethod(signMethod);
    setChoosingSignMethod(false);
  };

  const checkPayee = (e: React.ChangeEvent<HTMLInputElement>) => {
    setError('');

    const validPatPCommand = (validPatP: string) => {
      let command = { 'check-payee': validPatP };
      api.btcWalletCommand(command);
      setTimeout(() => {
        setCheckingPatp(false);
      }, 5000);
      setCheckingPatp(true);
      setPayeeType(payeeTypes.ship);
      setPayee(validPatP);
    };

    let payeeReceived = e.target.value;
    let isPatp = ob.isValidPatp(`~${deSig(payeeReceived)}`);
    let isAddress = validate(payeeReceived);
    if (isPatp) {
      validPatPCommand(`~${deSig(payeeReceived)}`);
    } else if (isAddress) {
      setPayee(payeeReceived);
      setReady(true);
      setCheckingPatp(false);
      setPayeeType(payeeTypes.address);
      setValidPayee(true);
    } else {
      setPayee(payeeReceived);
      setReady(false);
      setCheckingPatp(false);
      setPayeeType(payeeTypes.initial);
      setValidPayee(false);
    }
  };

  const toggleSignMethod = () => {
    setChoosingSignMethod(!choosingSignMethod);
  };

  const initPayment = () => {
    if (payeeType === payeeTypes.ship) {
      let command = {
        'init-payment': {
          payee,
          value: satsAmount,
          feyb: feeChoices[feeValue][1],
          note: note || null,
        },
      };

      api.btcWalletCommand(command).then(() => setSigning(true));
    } else if (payeeType === payeeTypes.address) {
      let command = {
        'init-payment-external': {
          address: payee,
          value: satsAmount,
          feyb: 1,
          note: note || null,
        },
      };
      api.btcWalletCommand(command).then(() => setSigning(true));
    }
  };

  useEffect(() => {
    if (network === 'bitcoin') {
      let url = 'https://bitcoiner.live/api/fees/estimates/latest';
      fetch(url)
        .then((res) => res.json())
        .then((n) => {
          // let estimates = Object.keys(n.estimates);
          // let mid = Math.floor(estimates.length / 2);
          // let high = estimates.length - 1;
          setFeeChoices({
            [feeLevels.high]: [30, n.estimates[30]['sat_per_vbyte']],
            [feeLevels.mid]: [180, n.estimates[180]['sat_per_vbyte']],
            [feeLevels.low]: [360, n.estimates[360]['sat_per_vbyte']],
          });
        });
    }
  }, []);

  useEffect(() => {
    if (!ready && !checkingPatp) {
      if (shipWallets.payee === payee.slice(1) && shipWallets.hasWallet) {
        setReady(true);
        setCheckingPatp(false);
        setValidPayee(true);
      }
    }
  }, [ready, checkingPatp, shipWallets, payee]);

  let payeeColor = 'black';
  let payeeBg = 'white';
  let payeeBorder = 'lightGray';
  if (error) {
    payeeColor = 'red';
    payeeBorder = 'red';
    payeeBg = 'veryLightRed';
  } else if (focusedField === focusFields.payee && validPayee) {
    payeeColor = 'green';
    payeeBorder = 'green';
    payeeBg = 'veryLightGreen';
  } else if (focusedField !== focusFields.payee && validPayee) {
    payeeColor = 'blue';
    payeeBorder = 'white';
    payeeBg = 'white';
  } else if (focusedField !== focusFields.payee && !validPayee) {
    payeeColor = 'red';
    payeeBorder = 'red';
    payeeBg = 'veryLightRed';
  } else if (
    focusedField === focusFields.payee &&
    !validPayee &&
    !checkingPatp &&
    payeeType === payeeTypes.ship
  ) {
    payeeColor = 'red';
    payeeBorder = 'red';
    payeeBg = 'veryLightRed';
  }

  const signReady = ready && satsAmount > 0 && !signing;

  let invoice = null;

  switch (signMethod) {
    case signMethods.masterTicket: {
      invoice = (
        <Invoice
          stopSending={stopSending}
          payee={payee}
          satsAmount={satsAmount}
        />
      );
      break;
    }
    case signMethods.bridge: {
      invoice = (
        <BridgeInvoice
          stopSending={stopSending}
          payee={payee}
          satsAmount={satsAmount}
        />
      );
      break;
    }
    case signMethods.external: {
      invoice = (
        <ExternalInvoice
          stopSending={stopSending}
          payee={payee}
          satsAmount={satsAmount}
        />
      );
      break;
    }
    default:
      break;
  }

  return (
    <>
      {signing && psbt ? (
        invoice
      ) : (
        <Col
          width="100%"
          backgroundColor="white"
          borderRadius="48px"
          mb={5}
          p={5}
        >
          <Col width="100%">
            <Row justifyContent="space-between" alignItems="center">
              <Text highlight color="blue" fontSize={1}>
                Send BTC
              </Text>
              <Text highlight color="blue" fontSize={1}>
                {value}
              </Text>
              <Icon icon="X" cursor="pointer" onClick={() => stopSending()} />
            </Row>
            <Row alignItems="center" mt={6} justifyContent="space-between">
              <Row
                justifyContent="space-between"
                width="calc(40% - 30px)"
                alignItems="center"
              >
                <Text gray fontSize={1} fontWeight="600">
                  To
                </Text>
                {checkingPatp ? (
                  <LoadingSpinner background="midOrange" foreground="orange" />
                ) : null}
              </Row>
              <Input
                // autoFocus
                onFocus={() => {
                  setFocusedField(focusFields.payee);
                }}
                onBlur={() => {
                  setFocusedField(focusFields.empty);
                }}
                color={payeeColor}
                backgroundColor={payeeBg}
                borderColor={payeeBorder}
                ml={2}
                flexGrow="1"
                fontSize="14px"
                placeholder="~sampel-palnet or BTC address"
                value={payee}
                fontFamily="mono"
                disabled={signing}
                onChange={(e: React.ChangeEvent<HTMLInputElement>) =>
                  checkPayee(e)
                }
              />
            </Row>
            {error && (
              <Row alignItems="center" justifyContent="space-between">
                {/* yes this is a hack */}
                <Box width="calc(40% - 30px)" />
                <Error error={error} fontSize="14px" />
              </Row>
            )}
            <Row alignItems="center" mt={4} justifyContent="space-between">
              <Text gray fontSize={1} fontWeight="600" width="40%">
                Amount
              </Text>
              <Input
                onFocus={() => {
                  setFocusedField(focusFields.currency);
                }}
                onBlur={() => {
                  setFocusedField(focusFields.empty);
                }}
                fontSize="14px"
                width="100%"
                type="number"
                borderColor={
                  focusedField === focusFields.currency ? 'lightGray' : 'none'
                }
                disabled={signing}
                value={denomAmount}
                onChange={(e: React.ChangeEvent<HTMLInputElement>) => {
                  setDenomAmount(parseFloat(e.target.value));
                  setSatsAmount(
                    Math.round(
                      (parseFloat(e.target.value) / conversion) * 100000000
                    )
                  );
                }}
              />
              <Text color="lighterGray" fontSize={1} ml={3}>
                {denomination}
              </Text>
            </Row>
            <Row alignItems="center" mt={2} justifyContent="space-between">
              {/* yes this is a hack */}
              <Box width="40%" />
              <Input
                onFocus={() => {
                  setFocusedField(focusFields.sats);
                }}
                onBlur={() => {
                  setFocusedField(focusFields.empty);
                }}
                fontSize="14px"
                width="100%"
                type="number"
                borderColor={
                  focusedField === focusFields.sats ? 'lightGray' : 'none'
                }
                disabled={signing}
                value={satsAmount}
                onChange={(e: React.ChangeEvent<HTMLInputElement>) => {
                  setDenomAmount(
                    parseFloat(e.target.value) * (conversion / 100000000)
                  );
                  setSatsAmount(parseInt(e.target.value, 10));
                }}
              />
              <Text color="lightGray" fontSize={1} ml={3}>
                sats
              </Text>
            </Row>
            <Row mt={4} width="100%" justifyContent="space-between">
              <Text gray fontSize={1} fontWeight="600" width="40%">
                Fee
              </Text>
              <Row
                alignItems="center"
                backgroundColor="blue"
                borderRadius="24px"
                paddingX="12px"
                paddingY="8px"
              >
                <Text mr={2} color="white" fontSize="14px">
                  {feeChoices[feeValue][1]} sats/vbyte
                </Text>
                <Button
                  borderRadius="24px"
                  height="24px"
                  width="24px"
                  border="none"
                  backgroundColor="rgba(33, 157, 255)"
                >
                  <Icon
                    icon="ChevronSouth"
                    width="12px"
                    color="white"
                    onClick={() => {
                      if (!showFeePicker) setShowFeePicker(true);
                    }}
                    cursor="pointer"
                  />
                </Button>
              </Row>
            </Row>
            <Col alignItems="center">
              {!showFeePicker ? null : (
                <FeePicker
                  feeChoices={feeChoices}
                  feeValue={feeValue}
                  setFeeValue={setFeeValue}
                  feeDismiss={feeDismiss}
                />
              )}
            </Col>
            <Row
              mt={4}
              width="100%"
              justifyContent="space-between"
              alignItems="center"
            >
              <Text gray fontSize={1} fontWeight="600" width="40%">
                Note
              </Text>
              <Input
                onFocus={() => {
                  setFocusedField(focusFields.note);
                }}
                onBlur={() => {
                  setFocusedField(focusFields.empty);
                }}
                fontSize="14px"
                width="100%"
                placeholder="What's this for?"
                type="text"
                borderColor={
                  focusedField === focusFields.note ? 'lightGray' : 'none'
                }
                disabled={signing}
                value={note}
                onChange={(e: React.ChangeEvent<HTMLInputElement>) => {
                  setNote(e.target.value);
                }}
              />
            </Row>
          </Col>
          <Row
            flexDirection="row"
            alignItems="center"
            mt={4}
            justifyContent="flex-end"
          >
            {!(signing && !error) ? null : (
              <LoadingSpinner background="midOrange" foreground="orange" />
            )}
            <Signer
              signReady={signReady}
              choosingSignMethod={choosingSignMethod}
              signMethod={signMethod}
              setSignMethod={handleSetSignMethod}
              initPayment={initPayment}
            />
            <Button
              ml={2}
              width="48px"
              fontSize={1}
              fontWeight="bold"
              borderRadius="24px"
              height="48px"
              onClick={() => toggleSignMethod()}
              color={signReady ? 'white' : 'lighterGray'}
              backgroundColor={
                signReady ? 'rgba(33, 157, 255, 0.2)' : 'veryLightGray'
              }
              disabled={!signReady}
              border="none"
              style={{ cursor: signReady ? 'pointer' : 'default' }}
            >
              <Icon
                icon="ChevronSouth"
                color={signReady ? 'blue' : 'lighterGray'}
              />
            </Button>
          </Row>
          {signMethod === signMethods.masterTicket && (
            <Row mt={4} alignItems="center">
              <Icon icon="Info" color="yellow" height={4} width={4} />
              <Text fontSize="14px" fontWeight="regular" color="gray" ml={2}>
                We recommend that you sign transactions using Bridge to protect
                your master ticket.
              </Text>
            </Row>
          )}
        </Col>
      )}
    </>
  );
};

export default Send;
