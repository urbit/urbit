import { Text } from '@tlon/indigo-react';
import { Contact, Content, Group } from '@urbit/api';
import React from 'react';
import { referenceToPermalink } from '~/logic/lib/permalinks';
import { cite, citeNickname, deSig } from '~/logic/lib/util';
import { useContact } from '~/logic/state/contact';
import { useShowNickname } from '~/logic/state/settings';
import { PropFunc } from '~/types';
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
} & PropFunc<typeof Text>) {
  const { ship, first = false, ...rest } = props;
  const contact = useContact(`~${deSig(ship)}`);
  const showNickname = useShowNickname(contact);
  const name = citeNickname(ship, showNickname, contact?.nickname);
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
        title={showNickname ? cite(ship) : contact?.nickname}
        {...rest}
      >
        {name}
      </Text>
    </ProfileOverlay>
  );
}
