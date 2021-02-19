import React, { ReactNode, useState, useRef } from 'react';
import moment from 'moment';
import { Row, Box, BaseImage } from '@tlon/indigo-react';
import { uxToHex, cite, useShowNickname } from '~/logic/lib/util';
import { Contacts } from '~/types/contact-update';
import OverlaySigil from './OverlaySigil';
import { Sigil } from '~/logic/lib/sigil';
import { Group } from '~/types';
import GlobalApi from '~/logic/api/global';
import { useHistory } from 'react-router-dom';

interface AuthorProps {
  contacts: Contacts;
  ship: string;
  date: number;
  showImage?: boolean;
  children?: ReactNode;
  unread?: boolean;
  group: Group;
  api: GlobalApi;
}

// eslint-disable-next-line max-lines-per-function
export default function Author(props: AuthorProps) {
  const { contacts, ship = '', date, showImage, group } = props;
  const history = useHistory();
  let contact;
  if (contacts) {
    contact = ship in contacts ? contacts[ship] : null;
  }
  const color = contact?.color ? `#${uxToHex(contact?.color)}` : '#000000';
  const showNickname = useShowNickname(contact);
  const name = showNickname ? contact.nickname : cite(ship);
  const dateFmt = moment(date).fromNow();

  const [showOverlay, setShowOverlay] = useState(false);

  const toggleOverlay = () => {
    setShowOverlay((value) => !value);
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
            history={history}
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
      <Box fontSize='1' ml={2} color={props.unread ? 'blue' : 'gray'}>
        {dateFmt}
      </Box>
      {props.children}
    </Row>
  );
}
