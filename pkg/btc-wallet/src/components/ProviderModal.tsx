import React, { useEffect, useState } from 'react';
import {
  Box,
  Text,
  Button,
  StatelessTextInput,
  Icon,
  Row,
  LoadingSpinner,
} from '@tlon/indigo-react';
import { isValidPatp } from 'urbit-ob';
import { api } from '../lib/api';
import { useSettings } from '../hooks/useSettings';

enum providerStatuses {
  checking,
  failed,
  ready,
  initial = '',
}

const ProviderModal = () => {
  const { providerPerms } = useSettings();
  const [providerStatus, setProviderStatus] = useState(
    providerStatuses.initial
  );
  const [potentialProvider, setPotentialProvider] = useState(null);
  const [provider, setProvider] = useState(null);
  const [connecting, setConnecting] = useState(false);

  const checkProvider = (e: React.ChangeEvent<HTMLInputElement>) => {
    // TODO: loading states
    setProviderStatus(providerStatuses.initial);
    let givenProvider = e.target.value;
    if (isValidPatp(givenProvider)) {
      let command = {
        'check-provider': givenProvider,
      };
      setPotentialProvider(givenProvider);
      setProviderStatus(providerStatuses.checking);
      api.btcWalletCommand(command);
      setTimeout(() => {
        setProviderStatus(providerStatuses.failed);
      }, 5000);
    }
    setProvider(givenProvider);
  };

  const submitProvider = () => {
    if (providerStatus === providerStatuses.ready) {
      let command = {
        'set-provider': provider,
      };
      api.btcWalletCommand(command);
      setConnecting(true);
    }
  };

  useEffect(() => {
    if (providerStatus !== providerStatuses.ready) {
      if (providerPerms.provider === provider && providerPerms.permitted) {
        setProviderStatus(providerStatuses.ready);
      }
    }
  }, [providerStatus, providerPerms, provider, setProviderStatus]);

  let workingNode = null;
  let workingColor = null;
  let workingBg = null;
  if (providerStatus === providerStatuses.ready) {
    workingColor = 'green';
    workingBg = 'veryLightGreen';
    workingNode = (
      <Box mt={3}>
        <Text fontSize="14px" color="green">
          {provider} is a working provider node
        </Text>
      </Box>
    );
  } else if (providerStatus === providerStatuses.failed) {
    workingColor = 'red';
    workingBg = 'veryLightRed';
    workingNode = (
      <Box mt={3}>
        <Text fontSize="14px" color="red">
          {potentialProvider} is not a working provider node
        </Text>
      </Box>
    );
  }

  return (
    <Box width="100%" height="100%" padding={3}>
      <Row>
        <Icon icon="Bitcoin" mr={2} />
        <Text fontSize="14px" fontWeight="bold">
          Step 1 of 2: Set up Bitcoin Provider Node
        </Text>
      </Row>
      <Box mt={3}>
        <Text fontSize="14px" fontWeight="regular" color="gray">
          In order to perform Bitcoin transaction in Landscape, you&apos;ll need
          to set a provider node. A provider node is an urbit which maintains a
          synced Bitcoin ledger.
          <a
            style={{ fontSize: '14px' }}
            target="_blank"
            href="https://urbit.org/bitcoin-wallet"
            rel="noreferrer"
          >
            {' '}
            Learn More
          </a>
        </Text>
      </Box>
      <Box mt={3} mb={2}>
        <Text fontSize="14px" fontWeight="500">
          Provider Node
        </Text>
      </Box>
      <Row alignItems="center">
        <StatelessTextInput
          mr={2}
          width="256px"
          fontSize="14px"
          type="text"
          name="masterTicket"
          placeholder="e.g. ~zod"
          autoCapitalize="none"
          autoCorrect="off"
          mono
          backgroundColor={workingBg}
          color={workingColor}
          borderColor={workingColor}
          onChange={(e: React.ChangeEvent<HTMLInputElement>) =>
            checkProvider(e)
          }
        />
        {providerStatus === providerStatuses.checking ? (
          <LoadingSpinner />
        ) : null}
      </Row>
      {workingNode}
      <Row alignItems="center" mt={3}>
        <Button
          mr={2}
          primary
          disabled={providerStatus !== providerStatuses.ready}
          fontSize="14px"
          style={{
            cursor:
              providerStatus === providerStatuses.ready ? 'pointer' : 'default',
          }}
          onClick={() => {
            submitProvider();
          }}
        >
          Set Peer Node
        </Button>
        {connecting ? <LoadingSpinner /> : null}
      </Row>
    </Box>
  );
};

export default ProviderModal;
