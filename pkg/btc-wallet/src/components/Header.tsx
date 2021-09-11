import React from 'react';
import { Box, Icon, Row, Text } from '@tlon/indigo-react';
import { Link } from 'react-router-dom';
import { useSettings } from '../hooks/useSettings';

const Header = () => {
  const { provider } = useSettings();

  let connection = null;
  if (!(provider && provider.connected)) {
    connection = (
      <Text fontSize={1} color="red" fontWeight="bold" mr={3}>
        Provider Offline
      </Text>
    );
  }

  return (
    <Row
      height={7}
      width="100%"
      justifyContent="space-between"
      alignItems="center"
      mb={5}
      px={[2, 4]}
      style={{
        boxShadow: '0px 1px 0px rgba(0, 0, 0, 0.1)',
      }}
    >
      <Row alignItems="center" justifyContent="center">
        <Box
          backgroundColor="orange"
          borderRadius={4}
          mr="12px"
          width={5}
          height={5}
          alignItems="center"
          justifyContent="center"
        >
          <Icon icon="Bitcoin" width={4} p={1} height={4} color="white" />
        </Box>
        <Text fontSize={2} fontWeight="bold" color="orange">
          Bitcoin
        </Text>
      </Row>
      <Row alignItems="center">
        {connection}
        <Link to="/~btc/settings">
          <Box
            backgroundColor="white"
            borderRadius={4}
            width={5}
            height={5}
            p={2}
            m={2}
            position="relative"
          >
            <Icon icon="Adjust" color="orange" />
          </Box>
        </Link>
        <a href="/">
          <Box
            backgroundColor="white"
            borderRadius={4}
            width={5}
            height={5}
            p={2}
            m={2}
            position="relative"
          >
            <Icon icon="Home" color="orange" />
          </Box>
        </a>
      </Row>
    </Row>
  );
};

export default Header;
