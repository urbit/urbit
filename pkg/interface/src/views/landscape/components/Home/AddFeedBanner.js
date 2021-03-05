import React, {
  useState,
  useEffect
} from 'react';
import _  from 'lodash';
import { Box, Row, Text, BaseImage } from '@tlon/indigo-react';
import { uxToHex } from '~/logic/lib/util';
import { Sigil } from '~/logic/lib/sigil';

export const AddFeedBanner = (props) => {
  const {
    api,
    group,
    groupPath,
  } = props;

  const onClick = async () => {
    //  TODO: implement api action to turn on group feed or dismiss
  };

  return (
    <Row
      height="48px"
      alignItems="center"
      justifyContent="space-between"
      borderBottom={1}
      borderColor="washedGray"
    >
      <Row pl={3} alignItems="center">
        <Text verticalAlign="middle" pl={2}>Enable Group Feed?</Text>
      </Row>
      <Box pr={2} onClick={onClick}>
        <Text color="blue" bold cursor="pointer">Dismiss</Text>
      </Box>
      <Box pr={2} onClick={onClick}>
        <Text color="blue" bold cursor="pointer">Enable Feed</Text>
      </Box>
    </Row>
  );
};
