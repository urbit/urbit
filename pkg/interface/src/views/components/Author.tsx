import React, { ReactElement, ReactNode, useState } from 'react';
import moment from 'moment';
import { useHistory } from 'react-router-dom';

import { Row, Box, BaseImage } from '@tlon/indigo-react';
import { Group, uxToHex, cite, deSig } from '@urbit/api';

import { useShowNickname } from '~/logic/lib/util';
import OverlaySigil from './OverlaySigil';
import { Sigil } from '~/logic/lib/Sigil';
import useContactState from '~/logic/state/contacts';
import Timestamp from './Timestamp';

interface AuthorProps {
  ship: string;
  date: number;
  showImage?: boolean;
  children?: ReactNode;
  unread?: boolean;
  group: Group;
}

// eslint-disable-next-line max-lines-per-function
export default function Author(props: AuthorProps): ReactElement {
  const { ship = '', date, showImage, group } = props;
  const history = useHistory();
  const contacts = useContactState(state => state.contacts);
  let contact;
  if (contacts) {
    contact = `~${deSig(ship)}` in contacts ? contacts[`~${deSig(ship)}`] : null;
  }
  const color = contact?.color ? `#${uxToHex(contact?.color)}` : '#000000';
  const showNickname = useShowNickname(contact);
  const name = showNickname ? contact.nickname : cite(ship);
  const stamp = moment(date);

  const [showOverlay, setShowOverlay] = useState(false);

  const toggleOverlay = () => {
    setShowOverlay(value => !value);
  };

  const img =
    contact && contact.avatar !== null ? (
      <BaseImage
        display='inline-block'
        src={contact.avatar}
        height={16}
        width={16}
      />
    ) : (
      <Sigil ship={ship} size={16} color={color} icon padding={2} />
    );

  return (
    <Row alignItems='center' width='auto'>
      <Box
        onClick={() => toggleOverlay()}
        height={16}
        position='relative'
        cursor='pointer'
      >
        {showImage && img}
        {showOverlay && (
          <OverlaySigil
            ship={ship}
            contact={contact}
            color={`#${uxToHex(contact?.color ?? '0x0')}`}
            group={group}
            onDismiss={() => toggleOverlay()}
            className='relative'
          />
        )}
      </Box>
      <Box
        ml={showImage ? 2 : 0}
        color='black'
        fontSize='1'
        lineHeight='tall'
        fontFamily={showNickname ? 'sans' : 'mono'}
        fontWeight={showNickname ? '500' : '400'}
      >
        {name}
      </Box>
      <Timestamp stamp={stamp} fontSize={1} time={false} ml={2} color={props.unread ? 'blue' : 'gray'} />
      {props.children}
    </Row>
  );
}
