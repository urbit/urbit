import React, { useState, useCallback } from 'react';
import _ from 'lodash';

import { Text, Box } from '@tlon/indigo-react';

import { Contact, Content, Group, cite, uxToHex } from '@urbit/api';

import RichText from '~/views/components/RichText';
import { useShowNickname } from '~/logic/lib/util';
import OverlaySigil from '~/views/components/OverlaySigil';
import useContactState from '~/logic/state/contacts';

interface MentionTextProps {
  contact?: Contact;
  content: Content[];
  group: Group;
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
        }
        return accum;
      }, '')}
    </RichText>
  );
}

export function Mention(props: {
  contact: Contact;
  group: Group;
  scrollWindow?: HTMLElement;
  ship: string;
}) {
  const { ship, scrollWindow } = props;
  const contacts = useContactState(state => state.contacts);
  let { contact } = props;
  contact = contact?.color ? contact : contacts?.[ship];
  const showNickname = useShowNickname(contact);
  const name = showNickname ? contact?.nickname : cite(ship);
  const group = props.group ?? { hidden: true };
  const [showOverlay, setShowOverlay] = useState(false);

  const toggleOverlay = useCallback(
    () => {
      setShowOverlay(value => !value);
    },
    [showOverlay]
  );

  return (
    <Box position='relative' display='inline-block' cursor='pointer'>
      <Text
        onClick={() => toggleOverlay()}
        mx='2px'
        px='2px'
        bg='washedBlue'
        color='blue'
        mono={!showNickname}
      >
        {name}
      </Text>
      {showOverlay && (
        <OverlaySigil
          ship={ship}
          contact={contact}
          color={`#${uxToHex(contact?.color ?? '0x0')}`}
          group={group}
          onDismiss={() => toggleOverlay()}
          className='relative'
          scrollWindow={scrollWindow}
        />
      )}
    </Box>
  );
}
