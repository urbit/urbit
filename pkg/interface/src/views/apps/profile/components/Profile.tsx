import React from "react";
import { Sigil } from "~/logic/lib/sigil";

import { uxToHex } from "~/logic/lib/util";
import {
  Center,
  Col,
  Box,
  Text,
  Row,
  BaseImage,
} from "@tlon/indigo-react";
import { AsyncButton } from "~/views/components/AsyncButton";
import GlobalApi from "~/logic/api/global";
import useLocalState from "~/logic/state/local";


const emptyContact = {
  nickname: '',
  bio: '',
  status: '',
  avatar: null,
  cover: null,
  'last-updated': 0
};

export function Profile(props: any) {
  const { hideAvatars, hideNicknames } = useLocalState(({ hideAvatars, hideNicknames }) => ({
    hideAvatars, hideNicknames
  }));
  if (!props.ship) {
    return null;
  }
  const { contact } = props;
  const hexColor = contact?.color ? `#${uxToHex(contact.color)}` : "#000000";
  const cover = (contact?.cover)
    ? <BaseImage src={contact.cover} width='100%' height='100%' style={{ objectFit: 'cover' }} />
    : <Box display="block" width='100%' height='100%' backgroundColor='washedGray' />;

  const image = (!hideAvatars && contact?.avatar)
    ? <BaseImage src={contact.avatar} width='100%' height='100%' style={{ objectFit: 'cover' }} />
    : <Sigil ship={`~${ship}`} size={96} color={hexColor} />;

  const nickname =
    (!hideNicknames && contact?.nickname) ? contact.nickname : "";

  return (
    <Center
      p={4}
      height="100%"
      width="100%"
      overflowY="auto">
      <Box
        maxWidth="600px"
        width="100%">
        <Row width="100%" height="300px">
          {cover}
        </Row>
        <Row
          pb={3}
          alignItems="center"
          width="100%"
        >
          <Center width="100%" marginTop="-48px">
            <Box height='96px' width='96px' borderRadius="2" overflow="hidden">
              {image}
            </Box>
          </Center>
          <Box ml={2}>
            <Text mono={!Boolean(nickname)}>{nickname}</Text>
          </Box>
        </Row>
      </Box>
    </Center>
  );
}
