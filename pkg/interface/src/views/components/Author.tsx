import React, { ReactElement, ReactNode, useState } from 'react';
import moment from 'moment';
import { useHistory } from 'react-router-dom';

import { Col, Row, Box, BaseImage } from '@tlon/indigo-react';
import { Contacts } from '@urbit/api/contacts';
import { Group } from '@urbit/api';

import { uxToHex, cite, useShowNickname, deSig } from '~/logic/lib/util';
import useSettingsState, {selectCalmState} from "~/logic/state/settings";
import useLocalState from "~/logic/state/local";
import { Sigil } from '~/logic/lib/sigil';
import Timestamp from './Timestamp';
import useContactState from '~/logic/state/contact';
import { useCopy } from '~/logic/lib/useCopy';
import ProfileOverlay from './ProfileOverlay';
import { PropFunc } from '~/types';

interface AuthorProps {
  ship: string;
  date: number;
  showImage?: boolean;
  children?: ReactNode;
  unread?: boolean;
  api?: GlobalApi;
  size?: number;
}

// eslint-disable-next-line max-lines-per-function
export default function Author(props: AuthorProps & PropFunc<typeof Box>): ReactElement {
  const {
    ship = '',
    date,
    showImage,
    fullNotIcon,
    children,
    unread,
    group,
    isRelativeTime,
    dontShowTime,
    ...rest
  } = props;

  const time = props.time || false;
  const size = props.size || 16;
  const sigilPadding = props.sigilPadding || 2;

  const history = useHistory();
  const osDark = useLocalState((state) => state.dark);

  const theme = useSettingsState(s => s.display.theme);
  const dark = theme === 'dark' || (theme === 'auto' && osDark)

  let contact;
  const contacts = useContactState(state => state.contacts);
  if (contacts) {
    contact = `~${deSig(ship)}` in contacts ? contacts[`~${deSig(ship)}`] : null;
  }
  const color = contact?.color ? `#${uxToHex(contact?.color)}` : dark ? '#000000' : '#FFFFFF';
  const showNickname = useShowNickname(contact);
  const { hideAvatars } = useSettingsState(selectCalmState);
  const name = showNickname && contact ? contact.nickname : cite(ship);
  const stamp = moment(date);
  const { copyDisplay, doCopy, didCopy } = useCopy(`~${ship}`, name);

  const [showOverlay, setShowOverlay] = useState(false);

  const toggleOverlay = () => {
    setShowOverlay(value => !value);
  };

  const sigil = fullNotIcon ? (
    <Sigil ship={ship} size={size} color={color} padding={sigilPadding} />
  ) : (
    <Sigil ship={ship} size={size} color={color} icon padding={sigilPadding} />
  );

  const img =
    contact?.avatar && !hideAvatars ? (
      <BaseImage
        referrerPolicy="no-referrer"
        display='inline-block'
        src={contact.avatar}
        style={{ objectFit: 'cover' }}
        height={size}
        width={size}
        borderRadius={1}
      />
    ) : sigil;

  return (
    <Row height="20px" {...rest} alignItems='center' width='auto'>
      <Box
        onClick={(e) => {
          e.stopPropagation();
          toggleOverlay();
        }}
        height={size}
        position='relative'
        cursor='pointer'
      >
        {showImage && (
          <ProfileOverlay ship={ship} api={props.api} >
            {img}
          </ProfileOverlay>
        )}
      </Box>
      <Box
        ml={showImage ? 2 : 0}
        color='black'
        fontSize='1'
        cursor='pointer'
        lineHeight='tall'
        fontFamily={showNickname ? 'sans' : 'mono'}
        fontWeight={showNickname ? '500' : '400'}
        onClick={doCopy}
      >
        {copyDisplay}
      </Box>
      { !dontShowTime && (
        <Timestamp
          relative={isRelativeTime}
          stamp={stamp}
          fontSize={1}
          time={time}
          ml={2}
          color={unread ? 'blue' : 'gray'} />
      )}
      {children}
    </Row>
  );
}
