import React, { useState } from 'react';
import {
  Box,
  Text,
  Button,
  StatelessTextInput,
  Icon,
  Row,
  LoadingSpinner,
} from '@tlon/indigo-react';
import { patp2dec, isValidPatq } from 'urbit-ob';
import * as kg from 'urbit-key-generation';
import { useSettings } from '../../hooks/useSettings';
import { api } from '../../api';

const WalletModal = () => {
  const { network } = useSettings();
  const [mode, setMode] = useState('xpub');
  const [masterTicket, setMasterTicket] = useState('');
  const [confirmedMasterTicket, setConfirmedMasterTicket] = useState('');
  const [xpub, setXpub] = useState('');
  const [readyToSubmit, setReadyToSubmit] = useState(false);
  const [processingSubmission, setProcessingSubmission] = useState(false);
  const [confirmingMasterTicket, setConfirmingMasterTicket] = useState(false);
  const [error, setError] = useState(false);

  const checkTicket = ({
    event: {
      target: { value },
    },
  }) => {
    // TODO: port over bridge ticket validation logic
    if (confirmingMasterTicket) {
      setConfirmedMasterTicket(value);
      setReadyToSubmit(isValidPatq(value));
    } else {
      setMasterTicket(value);
      setReadyToSubmit(isValidPatq(value));
    }
  };

  const checkXPub = ({ target: { value: xpubGiven } }) => {
    setXpub(xpubGiven);
    setReadyToSubmit(xpubGiven.length > 0);
  };

  const submitXPub = (givenXpub) => {
    const command = {
      'add-wallet': {
        xpub: givenXpub,
        fprint: [4, 0],
        'scan-to': null,
        'max-gap': 8,
        confs: 1,
      },
    };
    api.btcWalletCommand(command);
    setProcessingSubmission(true);
  };

  const submitMasterTicket = (ticket) => {
    setProcessingSubmission(true);
    kg.generateWallet({
      ticket,
      ship: parseInt(patp2dec('~' + window.ship)),
    }).then((urbitWallet) => {
      const { xpub: xpubFromWallet } =
        network === 'testnet'
          ? urbitWallet.bitcoinTestnet.keys
          : urbitWallet.bitcoinMainnet.keys;

      submitXPub(xpubFromWallet);
    });
  };

  const buttonDisabled = !readyToSubmit || processingSubmission;
  const inputDisabled = processingSubmission;
  // const processingSpinner = !processingSubmission ? null : <LoadingSpinner />;

  if (mode === 'masterTicket') {
    return (
      <Box width="100%" height="100%" padding={3}>
        <Row>
          <Icon icon="Bitcoin" mr={2} />
          <Text fontSize="14px" fontWeight="bold">
            Step 2 of 2: Import your extended public key
          </Text>
        </Row>
        <Row mt={3} alignItems="center">
          <Icon icon="Info" color="yellow" height={4} width={4} />
          <Text fontSize="14px" fontWeight="regular" color="gray" ml={2}>
            We recommend that you import your wallet using Bridge to protect
            your master ticket.
          </Text>
        </Row>
        <Box display="flex" alignItems="center" mt={3} mb={2}>
          {confirmingMasterTicket && (
            <Icon
              icon="ArrowWest"
              cursor="pointer"
              onClick={() => {
                setConfirmingMasterTicket(false);
                setMasterTicket('');
                setConfirmedMasterTicket('');
                setError(false);
              }}
            />
          )}
          <Text fontSize="14px" fontWeight="500">
            {confirmingMasterTicket ? 'Confirm Master Ticket' : 'Master Ticket'}
          </Text>
        </Box>
        <Row alignItems="center">
          <StatelessTextInput
            mr={2}
            width="256px"
            value={
              confirmingMasterTicket ? confirmedMasterTicket : masterTicket
            }
            disabled={inputDisabled}
            fontSize="14px"
            type="password"
            name="masterTicket"
            obscure={(value) => value.replace(/[^~-]+/g, '••••••')}
            placeholder="••••••-••••••-••••••-••••••"
            autoCapitalize="none"
            autoCorrect="off"
            onChange={(e) => checkTicket(e)}
          />
          {!inputDisabled ? null : <LoadingSpinner />}
        </Row>
        {error && (
          <Row mt={2}>
            <Text fontSize="14px" color="red">
              Master tickets do not match
            </Text>
          </Row>
        )}
        <Row mt={3}>
          <Button
            primary
            color="black"
            backgroundColor="veryLightGray"
            borderColor="veryLightGray"
            fontSize="14px"
            mr={2}
            style={{ cursor: 'pointer' }}
            onClick={() => {
              setMode('xpub');
              setMasterTicket('');
              setXpub('');
              setReadyToSubmit(false);
            }}
          >
            Cancel
          </Button>
          <Button
            primary
            disabled={buttonDisabled}
            fontSize="14px"
            style={{ cursor: buttonDisabled ? 'default' : 'pointer' }}
            onClick={() => {
              if (!confirmingMasterTicket) {
                setConfirmingMasterTicket(true);
              } else {
                if (masterTicket === confirmedMasterTicket) {
                  setError(false);
                  submitMasterTicket(masterTicket);
                } else {
                  setError(true);
                }
              }
            }}
          >
            Next Step
          </Button>
        </Row>
      </Box>
    );
  } else if (mode === 'xpub') {
    return (
      <Box width="100%" height="100%" padding={3}>
        <Row>
          <Icon icon="Bitcoin" mr={2} />
          <Text fontSize="14px" fontWeight="bold">
            Step 2 of 2: Import your extended public key
          </Text>
        </Row>
        <Box mt={3}>
          <Text fontSize="14px" fontWeight="regular" color="gray">
            Visit{' '}
            <a
              rel="noreferrer"
              href="https://bridge.urbit.org/?kind=xpub"
              target="_blank"
              style={{ color: 'black' }}
            >
              bridge.urbit.org
            </a>{' '}
            to obtain your key
          </Text>
        </Box>
        <Box mt={3} mb={2}>
          <Text fontSize="14px" fontWeight="500">
            Extended Public Key (XPub)
          </Text>
        </Box>
        <StatelessTextInput
          value={xpub}
          disabled={inputDisabled}
          fontSize="14px"
          type="password"
          name="xpub"
          autoCapitalize="none"
          autoCorrect="off"
          onChange={(e) => checkXPub(e)}
        />
        <Box mt={3} mb={3}>
          <Text
            fontSize="14px"
            fontWeight="regular"
            color={inputDisabled ? 'lighterGray' : 'gray'}
            style={{ cursor: inputDisabled ? 'default' : 'pointer' }}
            onClick={() => {
              if (inputDisabled) return;
              setMode('masterTicket');
              setXpub('');
              setMasterTicket('');
              setReadyToSubmit(false);
            }}
          >
            Import using master ticket -&gt;
          </Text>
        </Box>
        <Button
          primary
          mt={3}
          disabled={buttonDisabled}
          fontSize="14px"
          style={{ cursor: readyToSubmit ? 'pointer' : 'default' }}
          onClick={() => {
            submitXPub(xpub);
          }}
        >
          Next Step
        </Button>
      </Box>
    );
  }
};

export default WalletModal;
