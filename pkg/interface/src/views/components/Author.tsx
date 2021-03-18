import React, { ReactElement, ReactNode, useState } from 'react';
import moment from 'moment';
import { useHistory } from 'react-router-dom';

import { Col, Row, Box, BaseImage } from '@tlon/indigo-react';
import { Contacts } from '@urbit/api/contacts';
import { Group } from '@urbit/api';

import { uxToHex, cite, useShowNickname, deSig } from '~/logic/lib/util';
import useSettingsState, {selectCalmState} from "~/logic/state/settings";
import useLocalState from "~/logic/state/local";
import OverlaySigil from './OverlaySigil';
import { Sigil } from '~/logic/lib/sigil';
import GlobalApi from '~/logic/api/global';
import Timestamp from './Timestamp';

interface AuthorProps {
  contacts: Contacts;
  ship: string;
  date: number;
  showImage?: boolean;
  children?: ReactNode;
  unread?: boolean;
  group: Group;
  api?: GlobalApi;
  size?: number;
}

// eslint-disable-next-line max-lines-per-function
export default function Author(props: AuthorProps): ReactElement {
  const { contacts, ship = '', date, showImage, group } = props;

  const showAsCol = props.showAsCol || false;
  const time = props.time || false;
  const size = props.size || 16;
  const sigilPadding = props.sigilPadding || 2;
  const fullNotIcon = !!props.fullNotIcon;

  const history = useHistory();
  const osDark = useLocalState((state) => state.dark);

  const theme = useSettingsState(s => s.display.theme);
  const dark = theme === 'dark' || (theme === 'auto' && osDark)

  let contact;
  if (contacts) {
    contact = `~${deSig(ship)}` in contacts ? contacts[`~${deSig(ship)}`] : null;
  }
  const color = contact?.color ? `#${uxToHex(contact?.color)}` : dark ? '#000000' : '#FFFFFF';
  const showNickname = useShowNickname(contact);
  const { hideAvatars } = useSettingsState(selectCalmState);
  const name = showNickname ? contact.nickname : cite(ship);
  const stamp = moment(date);

  const [showOverlay, setShowOverlay] = useState(false);

  const toggleOverlay = () => {
    setShowOverlay(value => !value);
  };

  const sigil = fullNotIcon ? (
    <Sigil ship={ship} size={size} color={color} padding={sigilPadding} />
  ) : (
    <Sigil ship={ship} size={size} color={color} icon padding={sigilPadding} />
    );

  const rowOrCol = showAsCol ? (
    <Col>
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
      <Timestamp stamp={stamp} fontSize={1} time={time} ml={2} color={props.unread ? 'blue' : 'gray'} />
    </Col>
  ) : (
    <> 
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
      <Timestamp stamp={stamp} fontSize={1} time={time} ml={2} color={props.unread ? 'blue' : 'gray'} />
    </>
  );

  const img =
    contact?.avatar && !hideAvatars ? (
      <BaseImage
        display='inline-block'
        src={contact.avatar}
        style={{ objectFit: 'cover' }}
        height={size}
        width={size}
        borderRadius={1}
      />
    ) : sigil;

  return (
    <Row alignItems='center' width='auto'>
      <Box
        onClick={() => toggleOverlay()}
        height={size}
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
      {rowOrCol}
      {props.children}
    </Row>
  );
}
