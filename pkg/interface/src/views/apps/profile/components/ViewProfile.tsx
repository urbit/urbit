import React, {useEffect, useState} from "react";
import _ from 'lodash';
import { Sigil } from "~/logic/lib/sigil";

import {
  Center,
  Box,
  Text,
  Row,
  Button,
  Col,
  LoadingSpinner
} from "@tlon/indigo-react";
import { AsyncButton } from "~/views/components/AsyncButton";
import RichText from "~/views/components/RichText";
import { useHistory } from "react-router-dom";
import {GroupSummary} from "~/views/landscape/components/GroupSummary";
import {MetadataUpdatePreview} from "~/types";
import {GroupLink} from "~/views/components/GroupLink";
import {lengthOrder} from "~/logic/lib/util";


export function ViewProfile(props: any) {
  const history = useHistory();
  const { api, contact, nacked, isPublic, ship, associations, groups } = props;

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
          <Text mono color="darkGray">{ship}</Text>
        </Center>
      </Row>
      <Col
        pb={2}
        alignItems="center"
        justifyContent="center"
        width="100%">
          <Center flexDirection="column" maxWidth='32rem'>
          <RichText width='100%' disableRemoteContent>
            {(contact?.bio ? contact.bio : "")}
          </RichText>
          </Center>
        </Col>
      { (ship === `~${window.ship}`) ? (
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
      { (contact?.groups || []).length > 0 && (
        <Col gapY="3" mb="3" mt="6" alignItems="flex-start">
          <Text fontWeight="medium">Pinned Groups</Text>
          <Row flexWrap="wrap" justifyContent="flex-start" gapX="3">
            { contact?.groups.sort(lengthOrder).map(g => (
              <GroupLink
                mb="3"
                api={api}
                resource={g}
                groups={groups}
                associations={associations}
                measure={() => {}}
              />
            ))}
          </Row>
      </Col>
      )}
      
      { (nacked || (!isPublic && ship === `~${window.ship}`)) ? (
        <Box
          height="200px"
          borderRadius={1}
          bg="white"
          border={1}
          borderColor="washedGray">
          <Center height="100%">
            <Text mono pr={1} color="gray">{ship}</Text>
            <Text color="gray">remains private</Text>
          </Center>
        </Box>
      ) : null
      }
    </>
  );
}

