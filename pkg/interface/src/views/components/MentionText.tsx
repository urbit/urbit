import React, { useState, useCallback } from "react";
import _ from "lodash";
import { Text, Box } from "@tlon/indigo-react";
import {
  Contact,
  Contacts,
  Content,
  Group,
} from "~/types";
import RichText from "~/views/components/RichText";
import { cite, useShowNickname, uxToHex } from "~/logic/lib/util";
import ProfileOverlay from "./ProfileOverlay";
import { useHistory } from "react-router-dom";

interface MentionTextProps {
  contact?: Contact;
  contacts?: Contacts;
  content: Content[];
  group: Group;
}
export function MentionText(props: MentionTextProps) {
  const { content, contacts, contact, group, ...rest } = props;

  return (
    <RichText contacts={contacts} contact={contact} group={group} {...rest}>
      {content.reduce((accum, c) => {
        if ("text" in c) {
          return accum + c.text;
        } else if ("mention" in c) {
          return accum + `[~${c.mention}]`;
        }
        return accum;
      }, '')}
    </RichText>
  );
}

export function Mention(props: {
  ship: string;
  contact: Contact;
  contacts?: Contacts;
  group: Group;
}) {
  const { contacts, ship } = props;
  let { contact } = props;

  contact = (contact?.nickname) ? contact : contacts?.[ship];

  const showNickname = useShowNickname(contact);

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
