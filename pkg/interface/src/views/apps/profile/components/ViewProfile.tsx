import React from "react";
import { Sigil } from "~/logic/lib/sigil";

import {
  Center,
  Box,
  Text,
  Row,
  Button,
} from "@tlon/indigo-react";
import { AsyncButton } from "~/views/components/AsyncButton";
import RichText from "~/views/components/RichText";
import { useHistory } from "react-router-dom";


export function ViewProfile(props: any) {
  const history = useHistory();
  const { contact, ship } = props;

  return (
    <>
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
      { (ship === window.ship) ? (
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
    </>
  );
}

