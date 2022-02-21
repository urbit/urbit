import { BaseImage, Box, Row, Text } from '@tlon/indigo-react';
import moment from 'moment';
import React, { ReactElement, ReactNode } from 'react';
import { Sigil } from '~/logic/lib/sigil';
import { citeNickname, uxToHex } from '~/logic/lib/util';
import { useContact } from '~/logic/state/contact';
import { useDark } from '~/logic/state/join';
import useSettingsState, { selectCalmState, useShowNickname } from '~/logic/state/settings';
import { PropFunc } from '~/types';
import ProfileOverlay from './ProfileOverlay';
import Timestamp from './Timestamp';

export interface AuthorProps {
  ship: string;
  date?: number;
  showImage?: boolean;
  children?: ReactNode;
  unread?: boolean;
  size?: number;
  lineHeight?: string | number;
  isRelativeTime?: boolean;
  dontShowTime?: boolean;
  gray?: boolean;
}

// eslint-disable-next-line max-lines-per-function
function Author(props: AuthorProps & PropFunc<typeof Box>): ReactElement {
  const {
    ship = '',
    date,
    showImage,
    fullNotIcon,
    children,
    unread,
    isRelativeTime,
    dontShowTime,
    lineHeight = 1,
    gray = false,
    ...rest
  } = props;

  const time = props.time || props.date || false;
  const size = props.size || 16;
  const sigilPadding = props.sigilPadding || 2;

  const dark = useDark();

  const contact = useContact(ship);
  const color = contact?.color ? `#${uxToHex(contact?.color)}` : dark ? '#000000' : '#FFFFFF';
  const showNickname = useShowNickname(contact);
  const { hideAvatars } = useSettingsState(selectCalmState);
  const name = citeNickname(ship, showNickname, contact?.nickname);
  const stamp = moment(date);

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
    <Row {...rest} alignItems='center' width='auto'>
      <Box
        height={`${size}px`}
        position='relative'
        cursor='pointer'
      >
        <ProfileOverlay ship={ship}>
          <Row alignItems="center">
            {showImage && img}
            <Text
              ml={showImage ? 2 : 0}
              color={gray ? 'gray': 'black'}
              fontSize='1'
              cursor='pointer'
              lineHeight={lineHeight}
              fontFamily={showNickname ? 'sans' : 'mono'}
              fontWeight={showNickname ? '500' : '400'}
              mr={showNickname ? 0 : '2px'}
              mt={showNickname ? 0 : '0px'}
              overflow="hidden"
              textOverflow="ellipsis"
              whiteSpace="nowrap"
            >
              {name}
            </Text>
          </Row>
        </ProfileOverlay>
      </Box>
      <Box display='flex' alignItems='baseline'>
        { !dontShowTime && time && (
          <Timestamp
            height="fit-content"
            relative={isRelativeTime}
            stamp={stamp}
            fontSize={0}
            time={time}
            whiteSpace='nowrap'
            ml={2}
            color={unread ? 'blue' : 'gray'}
          />
        )}
        {children}
      </Box>
    </Row>
  );
}

export default React.memo(Author);
