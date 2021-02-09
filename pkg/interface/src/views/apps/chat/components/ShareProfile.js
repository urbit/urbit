import React, {
  useState,
  useEffect
} from 'react';
import _  from 'lodash';
import { Box, Row, Text, BaseImage } from '@tlon/indigo-react';
import { uxToHex } from '~/logic/lib/util';
import { Sigil } from '~/logic/lib/sigil';

const pathAsResource = (path) => {
  if (!path) {
    return false;
  }
  const pathArr = path.split('/');
  if (pathArr.length !== 4) {
    return false;
  }

  return {
    entity: pathArr[2],
    name: pathArr[3]
  };
};

export const ShareProfile = (props) => {
  const { api, hideBanner, group, groupPath } = props;

  const [showBanner, setShowBanner] = useState(false);
  const res = pathAsResource(groupPath);
  const [recipients, setRecipients] = useState([]);

  useEffect(() => {
    (async () => {
    if (!res) { return; }
    if (!group) { return; }
    if (group.hidden) {
      const members = _.compact(await Promise.all(
        Array.from(group.members)
          .map(s => {
            const ship = `~${s}`;
            if(s === window.ship) {
              return Promise.resolve(null);
            }
            return props.api.contacts.fetchIsAllowed(
              `~${window.ship}`,
              'personal',
              ship,
              true
            ).then(isAllowed => {
              return isAllowed ? null : ship;
            });
          })
      ));
      if(members.length > 0) {
        setShowBanner(true);
        setRecipients(members);
      } else {
        setShowBanner(false);
      }

    } else {
      const groupShared = await props.api.contacts.fetchIsAllowed(
        `~${window.ship}`,
        'personal',
        res.entity,
        true
      );
      setShowBanner(!groupShared);
    }
    })();

  }, [groupPath]);

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
        color={`#${uxToHex(props?.our?.color)}` || "#000000"}
        icon />
    </Row>
  );

  const onClick = async () => {
    if(group.hidden && recipients.length > 0) {
      await api.contacts.allowShips(recipients);
      await Promise.all(recipients.map(r => api.contacts.share(r)))
      setShowBanner(false);
    } else if (!group.hidden) {
      const [,,ship,name] = groupPath.split('/');
      await api.contacts.allowGroup(ship,name);
      await api.contacts.share(ship);
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
