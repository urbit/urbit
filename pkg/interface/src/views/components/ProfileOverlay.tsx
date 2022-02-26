import { BaseImage, Box, BoxProps, Button, Center, Col, Icon, Row, Text } from '@tlon/indigo-react';
import { uxToHex } from '@urbit/api';
import shallow from 'zustand/shallow';
import _ from 'lodash';
import React, { ReactNode, useCallback, useEffect, useMemo, useRef, useState } from 'react';
import { useHistory } from 'react-router-dom';
import VisibilitySensor from 'react-visibility-sensor';
import styled from 'styled-components';
import { getRelativePosition } from '~/logic/lib/relativePosition';
import { Sigil } from '~/logic/lib/sigil';
import { useCopy } from '~/logic/lib/useCopy';
import { useOutsideClick } from '~/logic/lib/useOutsideClick';
import { useContact } from '~/logic/state/contact';
import useSettingsState, { SettingsState, useShowNickname } from '~/logic/state/settings';
import { Portal } from './Portal';
import { ProfileStatus } from './ProfileStatus';
import RichText from './RichText';
import { citeNickname } from '~/logic/lib/util';
import { PalsProfileInfo } from './Pals/PalsProfileInfo';

export const OVERLAY_HEIGHT = 250;
const FixedOverlay = styled(Col)`
  position: fixed;
  -webkit-transition: all 0.1s ease-out;
  -moz-transition: all 0.1s ease-out;
  -o-transition: all 0.1s ease-out;
  transition: all 0.1s ease-out;
`;

export const ActionRow = styled(Row)`
  padding: 4px;
  width: 80%;
  cursor: pointer;
  &:hover {
    background-color: ${p => p.theme.colors.washedGray};
  }
`;

type ProfileOverlayProps = BoxProps & {
  ship: string;
  children?: ReactNode;
  color?: string;
};

const selSettings = (s: SettingsState) => [s.calm.hideAvatars, s.calm.hideNicknames];

const ProfileOverlay = (props: ProfileOverlayProps) => {
  const {
    ship,
    children,
    ...rest
  } = props;

  const [open, _setOpen] = useState(false);
  const [coords, setCoords] = useState({});
  const [visible, setVisible] = useState(false);
  const history = useHistory();
  const outerRef = useRef<HTMLDivElement>(null);
  const innerRef = useRef<HTMLDivElement>(null);
  const [hideAvatars, hideNicknames] = useSettingsState(selSettings, shallow);
  const isOwn = useMemo(() => window.ship === ship, [ship]);
  const { copyDisplay, doCopy, didCopy } = useCopy(`~${ship}`);

  const contact = useContact(`~${ship}`);
  const color = `#${uxToHex(contact?.color ?? '0x0')}`;
  const showNickname = useShowNickname(contact, hideNicknames);

  const setClosed = useCallback(() => {
    _setOpen(false);
  }, [_setOpen]);

  const setOpen = useCallback(() => {
    _setOpen(true);
  }, [_setOpen]);

  useEffect(() => {
    if(!visible) {
      setClosed();
    }
  }, [visible]);

  useOutsideClick(innerRef, setClosed);

  useEffect(() => {
    if(!open) {
      return () => {};
    }
    function _updateCoords() {
      if(outerRef.current) {
        const outer = outerRef.current;
        const { left, right, top } = outer.getBoundingClientRect();
        const spaceAtTop = top > 300;
        const spaceAtRight = right > 300 || right > left;
        setCoords(getRelativePosition(
          outer,
          spaceAtRight ? 'left' : 'right',
          spaceAtTop ? 'bottom' : 'top',
          -1* outer.clientWidth,
          -1 * outer.clientHeight
        ));
      }
    }
    const updateCoords = _.throttle(_updateCoords, 25);
    updateCoords();
    const interval = setInterval(updateCoords, 300);
    window.addEventListener('scroll', updateCoords);
    return () => {
      clearInterval(interval);
      window.removeEventListener('scroll', updateCoords);
    };
  }, [open]);

  const img =
    contact?.avatar && !hideAvatars ? (
      <BaseImage
        referrerPolicy='no-referrer'
        display='inline-block'
        style={{ objectFit: 'cover' }}
        src={contact.avatar}
        height={60}
        width={60}
        borderRadius={2}
      />
    ) : (
      <Box size={60} borderRadius={2} backgroundColor={color}>
        <Center height={60}>
          <Sigil ship={ship} size={32} color={color} />
        </Center>
      </Box>
    );

  return (
    <Box ref={outerRef} {...rest} onClick={setOpen} cursor="pointer">
      <VisibilitySensor active={open} onChange={setVisible}>
        {children}
      </VisibilitySensor>
  { open && (
    <Portal>
      <FixedOverlay
        ref={innerRef}
        {...coords}
        backgroundColor='white'
        color='washedGray'
        border={1}
        borderRadius={2}
        borderColor='lightGray'
        boxShadow='0px 0px 0px 3px'
        zIndex={3}
        fontSize={0}
        width='300px'
        padding={3}
        alignItems='center'
      >
        <Row
          height='60px'
          cursor='pointer'
          onClick={() => history.push(`/~profile/~${ship}`)}
          overflow='hidden'
          borderRadius={2}
        >
          <Box height="60px" width="60px">
            {img}
          </Box>
          <Col ml={2} alignSelf="center">
            <Row onClick={(e) => {
                  e.preventDefault();
                  e.stopPropagation();
                  doCopy();
                }}
            >
              <Text
                fontWeight='600'
                mono={!showNickname}
                textOverflow='ellipsis'
                overflow='hidden'
                whiteSpace='pre'
                marginBottom={0}
                cursor='pointer'
                display={didCopy ? 'none' : 'block'}
              >
                {citeNickname(ship, showNickname, contact?.nickname)}
              </Text>
              <Text
                fontWeight='600'
                marginBottom={0}
              >
                {copyDisplay}
              </Text>
            </Row>
            {isOwn ? (
              <ProfileStatus
                ship={`~${ship}`}
                contact={contact}
              />
            ) : (
              <RichText
                display='inline-block'
                minWidth={0}
                mb={0}
                disableRemoteContent
                gray
                title={contact?.status ? contact.status : ''}
              >
                {contact?.status ? contact.status : ''}
              </RichText>
            )}
          </Col>
        </Row>
        {!isOwn && (
          <>
            <ActionRow mt={2} onClick={() => history.push(`/~landscape/messages/dm/~${ship}`)}>
              <Icon icon='Chat' size={16} mr={2} />
              <Text>Message</Text>
            </ActionRow>
            <PalsProfileInfo ship={ship} />
          </>
        )}
      </FixedOverlay>
    </Portal>
    )}
  </Box>
  );
};

export default ProfileOverlay;
