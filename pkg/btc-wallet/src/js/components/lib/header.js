import React from 'react';
import { Box, Icon, Row, Text } from '@tlon/indigo-react';
import { Link } from 'react-router-dom';
import { useSettings } from '../../hooks/useSettings';

const Header = ({ settings }) => {
  const { provider } = useSettings();
  let icon = settings ? 'X' : 'Adjust';
  let iconColor = settings ? 'black' : 'orange';
  let iconLink = settings ? '/~btc' : '/~btc/settings';

  let connection = null;
  let badge = null;
  if (!(provider && provider.connected)) {
    connection = (
      <Text fontSize={1} color="red" fontWeight="bold" mr={3}>
        Provider Offline
      </Text>
    );

    if (!settings) {
      badge = (
        <Box
          borderRadius="50%"
          width="8px"
          height="8px"
          backgroundColor="red"
          position="absolute"
          top="0px"
          right="0px"
        ></Box>
      );
    }
  }

  return (
    <Row
      height={8}
      width="100%"
      justifyContent="space-between"
      alignItems="center"
      pt={5}
      pb={5}
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
        <Link to={iconLink}>
          <Box
            backgroundColor="white"
            borderRadius={4}
            width={5}
            height={5}
            p={2}
            position="relative"
          >
            {badge}
            <Icon icon={icon} color={iconColor} />
          </Box>
        </Link>
      </Row>
    </Row>
  );
};

export default Header;
