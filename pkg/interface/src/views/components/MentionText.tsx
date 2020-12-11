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
  group: Group;
  remoteContentPolicy: LocalUpdateRemoteContentPolicy;
  hideNicknames: boolean;
  hideAvatars: boolean;
}
export function MentionText(props: MentionTextProps) {
  const { content, contacts, group, hideNicknames } = props;

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
            <Mention key={idx} contacts={contacts || {}} group={group} ship={c.mention} hideNicknames={hideNicknames} hideAvatars={hideAvatars} />
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
  hideNicknames: boolean;
  hideAvatars: boolean;
}) {
  const { contacts, ship, hideNicknames, hideAvatars } = props;
  const contact = contacts[ship];
  const showNickname = (Boolean(contact?.nickname) && !hideNicknames);
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
        cursor="pointer"
      >
      {showOverlay && (
        <ProfileOverlay
          ship={ship}
          contact={contact}
          color={`#${uxToHex(contact?.color ?? '0x0')}`}
          group={group}
          onDismiss={onDismiss}
          hideAvatars={hideAvatars || false}
          hideNicknames={hideNicknames}
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
