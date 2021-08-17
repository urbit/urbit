import React from 'react';
import { Box, Text, Row } from '@tlon/indigo-react';

export const Tab = ({ selected, id, label, setSelected }) => (
  <Box
    py={2}
    borderBottom={1}
    borderBottomColor={selected === id ? 'black' : 'washedGray'}
    px={2}
    cursor='pointer'
    flexGrow={1}
    display="flex"
    alignItems="center"
    justifyContent="center"
    onClick={() => setSelected(id)}
  >
    <Text color={selected === id ? 'black' : 'gray'}>{label}</Text>
  </Box>
);

export const Tabs = ({ children, ...rest }: Parameters<typeof Row>[0]) => (
  <Row
    bg="white"
    mb={2}
    justifyContent="stretch"
    alignItems="flex-end"
    {...rest}
  >
    {children}
  </Row>
);

