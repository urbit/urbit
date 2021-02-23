import React, { createRef, PureComponent, useCallback, useEffect } from 'react';

import { Contact, Group, cite } from '@urbit/api';
import { useShowNickname } from '~/logic/lib/util';
import { Sigil } from '~/logic/lib/Sigil';

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
import useLocalState, { withLocalState } from '~/logic/state/local';
import { ProfileStatus } from './ProfileStatus';
import { useHistory } from 'react-router-dom';

export const OVERLAY_HEIGHT = 250;

type ProfileOverlayProps = ColProps & {
  ship: string;
  contact: Contact | null;
  color: string;
  topSpace: number | 'auto';
  bottomSpace: number | 'auto';
  group?: Group;
  onDismiss(): void;
};

const ProfileOverlay = (props: ProfileOverlayProps) => {
  const popoverRef = createRef<typeof Col>();

  const onDocumentClick = useCallback((event) => {
    // Do nothing if clicking ref's element or descendent elements
    if (!popoverRef.current || popoverRef?.current?.contains(event.target)) {
      return;
    }
    props.onDismiss();
  }, [popoverRef]);

  useEffect(() => {
    document.addEventListener('mousedown', onDocumentClick);
    document.addEventListener('touchstart', onDocumentClick);

    return () => {
      document.removeEventListener('mousedown', onDocumentClick);
      document.removeEventListener('touchstart', onDocumentClick);
    }
  }, [onDocumentClick]);

  const {
    contact,
    ship,
    color,
    topSpace,
    bottomSpace,
    onDismiss,
    ...rest
  } = props;

  const history = useHistory();
  const { hideNicknames, hideAvatars } = useLocalState(({ hideNicknames, hideAvatars }) => ({
    hideNicknames, hideAvatars
  }));

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

  const img = contact?.avatar && !hideAvatars
    ? <BaseImage display='inline-block' src={contact.avatar} height={72} width={72} className="brt2" />
    : <Sigil
      ship={ship}
      size={72}
      color={color}
      classes="brt2"
      svgClass="brt2"
      />;
  const showNickname = useShowNickname(contact, hideNicknames);

  return (
    <Col
      ref={popoverRef}
      backgroundColor="white"
      color="washedGray"
      border={1}
      borderRadius={2}
      borderColor="lightGray"
      boxShadow="0px 0px 0px 3px"
      position='absolute'
      zIndex={3}
      fontSize='0'
      height="250px"
      width="250px"
      padding={3}
      justifyContent="space-between"
      style={containerStyle}
      {...rest}
    >
      <Row color='black' width='100%' height="3rem">
        {(!isOwn) && (
        <Icon icon="Chat" size={16} onClick={() => history.push(`/~landscape/dm/${ship}`)} />
        )}
      </Row>
      <Box
        alignSelf="center"
        height="72px"
        onClick={() => history.push(`/~profile/~${ship}`)}
      >
        {img}
      </Box>
      <Col alignItems="end" justifyContent="flex-end" overflow="hidden" minWidth='0'>
        <Row width="100%" >
          <Text
            fontWeight='600'
            mono={!showNickname}
            textOverflow='ellipsis'
            overflow='hidden'
            whiteSpace='pre'
            lineHeight="tall"
          >
            {showNickname ? contact?.nickname : cite(ship)}
          </Text>
        </Row>
        { isOwn ? (
          <ProfileStatus
            ship={`~${ship}`}
            contact={contact}
          />
        ) : (
            <RichText display='inline-block' width='100%' minWidth='0' textOverflow='ellipsis'
              overflow='hidden'
              whiteSpace='pre'
              lineHeight="tall" disableRemoteContent gray>
            {contact?.status ? contact.status : ''}
          </RichText>
        )
        }
      </Col>
    </Col>
  );
};

export default ProfileOverlay;
