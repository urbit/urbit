import React from 'react';
import { Box, Icon, Row, Text } from '@tlon/indigo-react';
import { Sigil } from './sigil.js';

const TxCounterparty = ({ ship, address }) => {
  const icon = ship ? (
    <Sigil ship={ship} size={24} color="black" classes={''} icon padding={5} />
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
  const addressText = !address
    ? ''
    : address.slice(0, 6) + '...' + address.slice(-6);
  const text = ship ? `~${ship}` : addressText;

  return (
    <Row alignItems="center">
      {icon}
      <Text ml={2} mono fontSize="14px" color="gray">
        {text}
      </Text>
    </Row>
  );
};

export default TxCounterparty;
