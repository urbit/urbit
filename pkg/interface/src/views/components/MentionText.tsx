import { Text } from '@tlon/indigo-react';
import { Contact, Content, Group } from '@urbit/api';
import React from 'react';
import { referenceToPermalink } from '~/logic/lib/permalinks';
import { cite, deSig, useShowNickname } from '~/logic/lib/util';
import { useContact } from '~/logic/state/contact';
import ProfileOverlay from '~/views/components/ProfileOverlay';
import RichText from '~/views/components/RichText';

interface MentionTextProps {
  contact?: Contact;
  content: Content[];
  group: Group;
  transcluded: number;
}
export function MentionText(props: MentionTextProps) {
  const { content, contact, group, ...rest } = props;

  return (
    <RichText contact={contact} group={group} {...rest}>
      {content.reduce((accum, c) => {
        if ('text' in c) {
          return accum + c.text;
        } else if ('mention' in c) {
          return accum + `[~${c.mention}]`;
        } else if ('url' in c) {
          return accum + `\n ${c.url}`;
        } else if ('reference' in c) {
          const { link } = referenceToPermalink(c);
          return accum + `\n [${link}]`;
        }
        return accum;
      }, '')}
    </RichText>
  );
}

export function Mention(props: {
  ship: string;
  first?: boolean;
}) {
  const { ship, first = false } = props;
  const contact = useContact(`~${deSig(ship)}`);
  const showNickname = useShowNickname(contact);
  const name = showNickname ? contact?.nickname : cite(ship);

  return (
    <ProfileOverlay ship={ship} display="inline">
      <Text
        marginLeft={first? 0 : 1}
        marginRight={1}
        px={1}
        bg='washedBlue'
        color='blue'
        fontSize={showNickname ? 1 : 0}
        mono={!showNickname}
      >
        {name}
      </Text>
    </ProfileOverlay>
  );
}
