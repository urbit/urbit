import React, { useState, useCallback } from 'react';
import _ from 'lodash';
import { Text, Box } from '@tlon/indigo-react';
import { Contact, Contacts, Content, Group } from '@urbit/api';
import RichText from '~/views/components/RichText';
import { cite, useShowNickname, uxToHex } from '~/logic/lib/util';
import OverlaySigil from '~/views/components/OverlaySigil';
import { useHistory } from 'react-router-dom';
import useContactState from '~/logic/state/contact';

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
  first?: Boolean;
}) {
  const { ship, scrollWindow, first, ...rest } = props;
  let { contact } = props;
  const contacts = useContactState(state => state.contacts);
  contact = contact?.color ? contact : contacts?.[`~${ship}`];
  const history = useHistory();
  const showNickname = useShowNickname(contact);
  const name = showNickname ? contact?.nickname : cite(ship);
  const group = props.group ?? { hidden: true };
  const [showOverlay, setShowOverlay] = useState(false);

  const toggleOverlay = useCallback(() => {
    setShowOverlay((value) => !value);
  }, [showOverlay]);

  return (
    <Box position='relative' display='inline-block' cursor='pointer' {...rest}>
      <Text
        onClick={() => toggleOverlay()}
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
      {showOverlay && (
        <OverlaySigil
          ship={ship}
          contact={contact}
          color={`#${uxToHex(contact?.color ?? '0x0')}`}
          group={group}
          onDismiss={() => toggleOverlay()}
          history={history}
          className='relative'
          scrollWindow={scrollWindow}
        />
      )}
    </Box>
  );
}
