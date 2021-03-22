import React, { PureComponent, useCallback, useEffect, useRef } from 'react';
import { Contact, Group } from '@urbit/api';
import { cite, useShowNickname } from '~/logic/lib/util';
import { Sigil } from '~/logic/lib/sigil';

import {
  Box,
  Row,
  Col,
  Button,
  Text,
  BaseImage,
  ColProps,
  Icon
} from '@tlon/indigo-react';
import RichText from './RichText';
import { ProfileStatus } from './ProfileStatus';
import useSettingsState from '~/logic/state/settings';

export const OVERLAY_HEIGHT = 250;

type ProfileOverlayProps = ColProps & {
  ship: string;
  contact?: Contact;
  color: string;
  topSpace: number | 'auto';
  bottomSpace: number | 'auto';
  group?: Group;
  onDismiss(): void;
  hideAvatars: boolean;
  hideNicknames: boolean;
  history: any;
  api: any;
};

const ProfileOverlay = (props: ProfileOverlayProps) => {
  const {
    contact,
    ship,
    color,
    topSpace,
    bottomSpace,
    history,
    onDismiss,
    ...rest
  } = props;
  const hideAvatars = useSettingsState(state => state.calm.hideAvatars);
  const hideNicknames = useSettingsState(state => state.calm.hideNicknames);
  const popoverRef = useRef<typeof Col>(null);

  const onDocumentClick = useCallback((event) => {
    if (!popoverRef.current || popoverRef?.current?.contains(event.target)) {
      return;
    }
    onDismiss();
  }, [onDismiss, popoverRef]);

  useEffect(() => {
    document.addEventListener('mousedown', onDocumentClick);
    document.addEventListener('touchstart', onDocumentClick);

    return () => {
      document.removeEventListener('mousedown', onDocumentClick);
      document.removeEventListener('touchstart', onDocumentClick);
    }
  }, [onDocumentClick]);

  let top, bottom;
    if (topSpace < OVERLAY_HEIGHT / 2) {
      top = '0px';
    }
    if (bottomSpace < OVERLAY_HEIGHT / 2) {
      bottom = '0px';
    }
    if (!(top || bottom)) {
      bottom = `-${Math.round(OVERLAY_HEIGHT / 2)}px`;
    }
    const containerStyle = { top, bottom, left: '100%' };

    const isOwn = window.ship === ship;

    const img =
      contact?.avatar && !hideAvatars ? (
        <BaseImage
          display='inline-block'
          style={{ objectFit: 'cover' }}
          src={contact.avatar}
          height={72}
          width={72}
          borderRadius={2}
        />
      ) : (
        <Sigil ship={ship} size={72} color={color} />
      );
    const showNickname = useShowNickname(contact, hideNicknames);

    return (
      <Col
        ref={popoverRef}
        backgroundColor='white'
        color='washedGray'
        border={1}
        borderRadius={2}
        borderColor='lightGray'
        boxShadow='0px 0px 0px 3px'
        position='absolute'
        zIndex='3'
        fontSize='0'
        height='250px'
        width='250px'
        padding={3}
        justifyContent='center'
        style={containerStyle}
        {...rest}
      >
        <Row color='black' padding={3} position='absolute' top={0} left={0}>
          {!isOwn && (
            <Icon
              icon='Chat'
              size={16}
              cursor='pointer'
              onClick={() => history.push(`/~landscape/dm/${ship}`)}
            />
          )}
        </Row>
        <Box
          alignSelf='center'
          height='72px'
          cursor='pointer'
          onClick={() => history.push(`/~profile/~${ship}`)}
          overflow='hidden'
          borderRadius={2}
        >
          {img}
        </Box>
        <Col
          position='absolute'
          overflow='hidden'
          minWidth='0'
          width='100%'
          padding={3}
          bottom={0}
          left={0}
        >
          <Row width='100%'>
            <Text
              fontWeight='600'
              mono={!showNickname}
              textOverflow='ellipsis'
              overflow='hidden'
              whiteSpace='pre'
              marginBottom='0'
            >
              {showNickname ? contact?.nickname : cite(ship)}
            </Text>
          </Row>
          {isOwn ? (
            <ProfileStatus
              api={props.api}
              ship={`~${ship}`}
              contact={contact}
            />
          ) : (
            <RichText
              display='inline-block'
              width='100%'
              minWidth='0'
              textOverflow='ellipsis'
              overflow='hidden'
              whiteSpace='pre'
              marginBottom='0'
              disableRemoteContent
              gray
            >
              {contact?.status ? contact.status : ''}
            </RichText>
          )}
        </Col>
      </Col>
    );
};

export default ProfileOverlay;