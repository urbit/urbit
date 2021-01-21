import React from "react";
import { Sigil } from "~/logic/lib/sigil";

import { uxToHex } from "~/logic/lib/util";
import {
  Center,
  Box,
  Text,
  Row,
  BaseImage,
  Button,
} from "@tlon/indigo-react";
import { AsyncButton } from "~/views/components/AsyncButton";
import RichText from "~/views/components/RichText";
import GlobalApi from "~/logic/api/global";
import useLocalState from "~/logic/state/local";
import { useHistory } from "react-router-dom";


const emptyContact = {
  nickname: '',
  bio: '',
  status: '',
  avatar: null,
  cover: null,
  'last-updated': 0
};

export function Profile(props: any) {
  const history = useHistory();
  const { hideAvatars, hideNicknames } = useLocalState(({ hideAvatars, hideNicknames }) => ({
    hideAvatars, hideNicknames
  }));
  if (!props.ship) {
    return null;
  }
  const { contact, isEdit} = props;
  const hexColor = contact?.color ? `#${uxToHex(contact.color)}` : "#000000";
  const cover = (contact?.cover)
    ? <BaseImage src={contact.cover} width='100%' height='100%' style={{ objectFit: 'cover' }} />
    : <Box display="block" width='100%' height='100%' backgroundColor='washedGray' />;

  const image = (!hideAvatars && contact?.avatar)
    ? <BaseImage src={contact.avatar} width='100%' height='100%' style={{ objectFit: 'cover' }} />
    : <Sigil ship={`~${ship}`} size={96} color={hexColor} />;

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
          pb={2}
          alignItems="center"
          width="100%"
        >
          <Center width="100%" marginTop="-48px">
            <Box height='96px' width='96px' borderRadius="2" overflow="hidden">
              {image}
            </Box>
          </Center>
        </Row>
        <Row
          pb={2}
          alignItems="center"
          width="100%">
          <Center width="100%">
            <Text>
              {(contact?.nickname ? contact.nickname : "")}
            </Text>
          </Center>
        </Row>
        <Row
          pb={2}
          alignItems="center"
          width="100%">
          <Center width="100%">
            <Text mono color="darkGray">
              {`~${ship}`}
            </Text>
          </Center>
        </Row>
        <Row
          pb={2}
          alignItems="center"
          width="100%">
          <Center width="100%">
            <RichText>
              {(contact?.bio ? contact.bio : "")}
            </RichText>
          </Center>
        </Row>
        { (ship === window.ship && !isEdit) ? (
            <Row
              pb={2}
              alignItems="center"
              width="100%">
              <Center width="100%">
                <Button
                  backgroundColor="black"
                  color="white"
                  onClick={() => {history.push(`/~profile/${ship}/edit`)}}>
                  Edit Profile
                </Button>
              </Center>
            </Row>
          ) : null 
        }
        <Box
          height="200px"
          borderRadius={1}
          bg="white"
          border={1}
          borderColor="washedGray">
          <Center height="100%">
            <Text mono pr={1} color="gray">{`~${ship} `}</Text>
            <Text color="gray">remains private</Text>
          </Center>
        </Box>
      </Box>
    </Center>
  );
}
