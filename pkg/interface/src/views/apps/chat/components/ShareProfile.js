import React, {
  useState,
  useEffect
} from 'react';
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
  const { api, recipient, hideBanner, group, groupPath } = props;
  console.log(groupPath);
  //  TODO: use isContactPublic somewhere

  const [showBanner, setShowBanner] = useState(false);
  const res = pathAsResource(groupPath);

  useEffect(() => {
    if (!res) { return; }
    if (!group) { return; }
    console.log(group);
    if (group.hidden) {
      // TODO:
      // take the union of the pending set and the members set,
      // subtract ourselves, then if *anyone* has not already been shared with,
      // show the banner
      // Promise.all all the members of the set
      let check = 
      Promise.all()
      props.api.contacts.fetchIsAllowed(
        `~${window.ship}`,
        'personal',  // not used
        recipient,
        true
      ).then((retVal) => {
        console.log(retVal);
        setShowBanner(!retVal);
      });
    } else {
      // TODO:
      // if the group is not in the allowed-groups set, then show the banner
      props.api.contacts.fetchIsAllowed(
        res.entity,
        res.name,
        recipient,
        false
      ).then((retVal) => {
        console.log(retVal);
        setShowBanner(!retVal);
      });
    }
  }, [recipient, res, group]);

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
      backgroundColor={!props.our ? `#${uxToHex(props.our.color)}` : "#000000"}>
      <Sigil
        ship={window.ship}
        size={16}
        color={`#${uxToHex(props?.our?.color)}` || "#000000"}
        icon />
    </Row>
  );

  const onClick = () => {
    api.contacts.allow(recipient).then(() => {
      api.contacts.share(recipient, window.ship);
    });
    hideBanner();
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
