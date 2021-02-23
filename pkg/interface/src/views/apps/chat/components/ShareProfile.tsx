import React from 'react';
import _ from 'lodash';

import { Box, Row, Text, BaseImage } from '@tlon/indigo-react';
import { uxToHex, share, allowGroup, allowShips } from '@urbit/api';

import { Sigil } from '~/logic/lib/Sigil';
import useApi from '~/logic/lib/useApi';

export const ShareProfile = (props) => {
  const {
    showBanner,
    setShowBanner,
    group,
    groupPath,
    recipients
  } = props;

  const image = (props?.our?.avatar)
  ? (
    <BaseImage
      src={props.our.avatar}
      width='24px'
      height='24px'
      borderRadius={2}
      style={{ objectFit: 'cover' }} />
  ) : (
    <Row
      p={1}
      alignItems="center"
      borderRadius={2}
      backgroundColor={!!props.our ? `#${uxToHex(props.our.color)}` : "#000000"}>
      <Sigil
        ship={window.ship}
        size={16}
        color={!!props.our ? `#${uxToHex(props.our.color)}` : "#000000"}
        icon />
    </Row>
  );

  const onClick = async () => {
    const api = useApi();
    if (group.hidden && recipients.length > 0) {
      await api.poke(allowShips(recipients));
      await Promise.all(recipients.map(r => api.poke(share(r))))
      setShowBanner(false);
    } else if (!group.hidden) {
      const [, , ship, name] = groupPath.split('/');
      await api.poke(allowGroup(ship, name))
      if (ship !== `~${window.ship}`) {
        await api.poke(share(ship));
      }
      setShowBanner(false);
    }
  };

  return showBanner ? (
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
      <Box pr={2} onClick={onClick}>
        <Text color="blue" bold cursor="pointer">Share</Text>
      </Box>
    </Row>
  ) : null;
};
