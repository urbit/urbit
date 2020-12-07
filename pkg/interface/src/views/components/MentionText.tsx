import React, { useState, useCallback } from "react";
import _ from "lodash";
import { Text, Box } from "@tlon/indigo-react";
import {
  Contacts,
  Content,
  LocalUpdateRemoteContentPolicy,
  Group,
} from "~/types";
import RichText from "~/views/components/RichText";
import { cite, uxToHex } from "~/logic/lib/util";
import { ProfileOverlay } from "./ProfileOverlay";
import {useHistory} from "react-router-dom";

interface MentionTextProps {
  contacts: Contacts;
  content: Content[];
  remoteContentPolicy: LocalUpdateRemoteContentPolicy;
}
export function MentionText(props: MentionTextProps) {
  const { content, contacts } = props;

  return (
    <>
      {_.map(content, (c, idx) => {
        if ("text" in c) {
          return (
            <RichText
              inline
              key={idx}
              remoteContentPolicy={props.remoteContentPolicy}
            >
              {c.text}
            </RichText>
          );
        } else if ("mention" in c) {
          return (
            <Mention key={idx} contacts={contacts || {}} ship={c.mention} />
          );
        }
        return null;
      })}
    </>
  );
}

export function Mention(props: {
  ship: string;
  contacts: Contacts;
  group: Group;
}) {
  const { contacts, ship } = props;
  const contact = contacts[ship];
  const showNickname = !!contact?.nickname;
  const name = showNickname ? contact?.nickname : cite(ship);
  const [showOverlay, setShowOverlay] = useState(false);
  const onDismiss = useCallback(() => {
    setShowOverlay(false);
  }, [setShowOverlay]);
  const onClick = useCallback(() => {
    setShowOverlay(true);
  }, [setShowOverlay]);

  const group = props.group ?? { hidden: true };

  const history = useHistory();

  return (
      <Box
        position="relative"
        display="inline-block"
      >
      {showOverlay && (
        <ProfileOverlay
          ship={ship}
          contact={contact}
          color={uxToHex(contact?.color ?? '0x0')}
          group={group}
          onDismiss={onDismiss}
          hideAvatars={false}
          hideNicknames={false}
          history={history}
        />
      )}
      <Text
        onClick={onClick}
        mx="2px"
        px="2px"
        bg="washedBlue"
        color="blue"
        mono={!showNickname}
      >
        {name}
      </Text>
    </Box>
  );
}
