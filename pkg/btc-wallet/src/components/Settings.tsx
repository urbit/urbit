import React from 'react';
import { Row, Text, Button, Col } from '@tlon/indigo-react';
import { useSettings } from '../hooks/useSettings';
import { api } from '../lib/api';

const Settings = () => {
  const { wallet, provider } = useSettings();

  const changeProvider = () => {
    api.btcWalletCommand({ 'set-provider': null });
    window.location.reload();
  };

  const replaceWallet = () => {
    api.btcWalletCommand({
      'delete-wallet': wallet,
    });
  };

  let connColor = 'red';
  let connBackground = 'veryLightRed';
  let conn = 'Offline';
  let host = '';
  if (provider) {
    if (provider.connected) conn = 'Connected';
    if (provider.host) host = provider.host;
    if (provider.connected && provider.host) {
      connColor = 'orange';
      connBackground = 'lightOrange';
    }
  }

  return (
    <Col
      display="flex"
      width="100%"
      p={5}
      mb={5}
      borderRadius="48px"
      backgroundColor="white"
    >
      <Row mb="12px">
        <Text fontSize={1} fontWeight="bold" color="black">
          XPub Derivation
        </Text>
      </Row>
      <Row
        borderRadius="12px"
        backgroundColor="veryLightGray"
        py={5}
        px="36px"
        mb="12px"
        alignItems="center"
        justifyContent="space-between"
      >
        <Text mono fontSize={1} style={{ wordBreak: 'break-all' }} color="gray">
          {wallet}
        </Text>
      </Row>
      <Row width="100%" mb={5}>
        <Button
          width="100%"
          fontSize={1}
          fontWeight="bold"
          backgroundColor="gray"
          color="white"
          borderColor="none"
          borderRadius="12px"
          p={4}
          onClick={() => replaceWallet()}
        >
          Replace Wallet
        </Button>
      </Row>
      <Row mb="12px">
        <Text fontSize={1} fontWeight="bold" color="black">
          BTC Node Provider
        </Text>
      </Row>
      <Col
        mb="12px"
        py={5}
        px="36px"
        borderRadius="12px"
        backgroundColor={connBackground}
        alignItems="center"
        justifyContent="space-between"
      >
        <Text fontSize={1} color={connColor} mono>
          ~{host}
        </Text>
        <Text fontSize={0} color={connColor}>
          {conn}
        </Text>
      </Col>
      <Row width="100%">
        <Button
          width="100%"
          fontSize={1}
          fontWeight="bold"
          backgroundColor="orange"
          color="white"
          borderColor="none"
          borderRadius="12px"
          p={4}
          onClick={() => changeProvider()}
        >
          Change Provider
        </Button>
      </Row>
    </Col>
  );
};

export default Settings;
