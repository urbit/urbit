import React from 'react';
import { Box, Text, Button, Col, Anchor } from '@tlon/indigo-react';
import { api } from '../lib/api';
import { useSettings } from '../hooks/useSettings';

const Warning = () => {
  const { setShowWarning } = useSettings();
  const understand = () => {
    setShowWarning(false);
    let removeWarning = {
      'put-entry': {
        value: false,
        'entry-key': 'warning',
        'bucket-key': 'btc-wallet',
      },
    };
    api.settingsEvent(removeWarning);
  };

  return (
    <Box
      backgroundColor="red"
      color="white"
      borderRadius="32px"
      justifyContent="space-between"
      width="100%"
      p={5}
      mb={5}
    >
      <Col>
        <Text color="white" fontWeight="bold" fontSize={1}>
          Warning!
        </Text>
        <br />
        <Text color="white" fontWeight="bold" fontSize={1}>
          Be safe while using this wallet, and be sure to store responsible
          amounts of BTC.
        </Text>
        <Text color="white" fontWeight="bold" fontSize={1}>
          Always ensure that the checksum of the wallet matches that of the
          wallet&apos;s repo.
        </Text>
        <br />
        <Anchor href="https://urbit.org/bitcoin-wallet" target="_blank">
          <Text
            color="white"
            fontWeight="bold"
            fontSize={1}
            style={{ textDecoration: 'underline' }}
          >
            Learn more on urbit.org
          </Text>
        </Anchor>
      </Col>
      <Button
        backgroundColor="white"
        fontSize={1}
        mt={5}
        color="red"
        fontWeight="bold"
        borderRadius="24px"
        p="24px"
        borderColor="none"
        onClick={() => understand()}
      >
        I understand
      </Button>
    </Box>
  );
};

export default Warning;
