import React from 'react';
import { Box, Row, Text, BaseImage } from '@tlon/indigo-react';
import { uxToHex } from '~/logic/lib/util';
import { Sigil } from '~/logic/lib/sigil';

export const ShareProfile = (props) => {
  const image = (props?.our?.avatar)
  ? <BaseImage src={props.our.avatar} width='24px' height='24px' borderRadius={2} style={{ objectFit: 'cover' }} />
  : <Row p={1} alignItems="center" borderRadius={2} backgroundColor={`#${uxToHex(props?.our?.color)}` || "#000000"}>
      <Sigil ship={window.ship} size={16} icon color={`#${uxToHex(props?.our?.color)}` || "#000000"} />
    </Row>;

  return (
    <Row
      height="48px"
      alignItems="center"
      justifyContent="space-between"
      borderBottom={1}
      borderColor="washedGray"
    >
      <Row pl={3} alignItems="center">
        {image}
        <Text verticalAlign="middle" pl={2}>Share private profile?</Text>
      </Row>
      <Box pr={2}>
        <Text color="blue" bold cursor="pointer">Share</Text>
      </Box>
    </Row>
  );
};
