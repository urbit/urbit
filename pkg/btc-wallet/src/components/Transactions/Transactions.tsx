import React from 'react';
import { Box, Text, Col } from '@tlon/indigo-react';
import Transaction from './Transaction';
import { useSettings } from '../../hooks/useSettings';

const Transactions = () => {
  const { history } = useSettings();
  if (!history || history.length <= 0) {
    return (
      <Box
        alignItems="center"
        display="flex"
        justifyContent="center"
        height="340px"
        width="100%"
        p={5}
        mb={5}
        borderRadius="48px"
        backgroundColor="white"
      >
        <Text color="gray" fontSize={2} fontWeight="bold">
          No Transactions Yet
        </Text>
      </Box>
    );
  } else {
    return (
      <Col
        width="100%"
        backgroundColor="white"
        borderRadius="48px"
        mb={5}
        p={5}
      >
        {history.map((tx, i) => {
          return <Transaction tx={tx} key={i} />;
        })}
      </Col>
    );
  }
};

export default Transactions;
