import React from "react";
import _ from "lodash";
import { Text } from "@tlon/indigo-react";
import { Contacts, Content, LocalUpdateRemoteContentPolicy } from "~/types";
import RichText from "~/views/components/RichText";
import { cite } from "~/logic/lib/util";

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

function Mention(props: { ship: string; contacts: Contacts }) {
  const { contacts, ship } = props;
  const contact = contacts[ship];
  const showNickname = !!contact?.nickname;
  const name = showNickname ? contact?.nickname : cite(ship);

  return (
    <Text mx="2px" px="2px" bg="washedBlue" color="blue" mono={!showNickname}>
      {name}
    </Text>
  );
}
