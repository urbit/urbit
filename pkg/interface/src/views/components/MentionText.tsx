import { Text } from '@tlon/indigo-react';
import { Contact, Content, Group } from '@urbit/api';
import React from 'react';
import { referenceToPermalink } from '~/logic/lib/permalinks';
import { deSig } from '~/logic/lib/util';
import { PropFunc } from '~/types';
import ProfileOverlay from '~/views/components/ProfileOverlay';
import RichText from '~/views/components/RichText';
import { ShipName } from './ShipName';

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
  return (
    <ProfileOverlay ship={ship} display="inline">
      <ShipName
        ship={`~${deSig(ship)}`}
        marginLeft={first? 0 : 1}
        marginRight={1}
        px={1}
        bg='washedBlue'
        color='blue'
        cursor="pointer"
        {...rest}
      />
    </ProfileOverlay>
  );
}
