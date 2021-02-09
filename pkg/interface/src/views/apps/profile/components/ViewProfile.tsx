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


export function ViewProfile(props: any) {
  const history = useHistory();
  const { api, contact, nacked, isPublic, ship } = props;

  const [previews, setPreviews] = useState<MetadataUpdatePreview[]>([]);
  useEffect(() => {
    (async () => {
      setPreviews(
        _.compact(
          await Promise.all(
            (contact?.groups || []).map(g => api.metadata.preview(g)
              .catch(() => null)
            )
          )
        )
      );
    })();

    return () => {
      setPreviews([]);
    }
  }, [ship]);

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
          <RichText width='100%'>
            {(contact?.bio ? contact.bio : "")}
          </RichText>
          </Center>
        </Col>
      { (contact?.groups || []).length > 0 && (
        <Col gapY="3" my="3" alignItems="center">
          <Text fontWeight="medium">Pinned Groups</Text>
           {previews.length === 0 ? (
            <LoadingSpinner />
            ) : (
              <Row justifyContent="center" gapX="3">
                {previews.map(p => (
                <Box p="2" border="1" borderColor="washedGray">
                  <GroupSummary
                    metadata={p.metadata}
                    memberCount={p.members}
                    channelCount={p?.['channel-count']}
                  />
                </Box>
              ))}
          </Row>
          )}
      </Col>
      )}
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

