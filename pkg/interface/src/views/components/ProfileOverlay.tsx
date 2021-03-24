import React, { PureComponent, useCallback, useEffect, useRef, useState, useMemo } from 'react';
import { Contact, Group, uxToHex } from '@urbit/api';
import VisibilitySensor from 'react-visibility-sensor';
import styled from 'styled-components';

import { cite, useShowNickname } from '~/logic/lib/util';
import { Sigil } from '~/logic/lib/sigil';

import {
  Box,
  Row,
  Col,
  Button,
  Text,
  BaseImage,
  Icon,
  BoxProps,
  ColProps,
  Center
} from '@tlon/indigo-react';
import RichText from './RichText';
import { ProfileStatus } from './ProfileStatus';
import useSettingsState from '~/logic/state/settings';
import {useOutsideClick} from '~/logic/lib/useOutsideClick';
import {useContact} from '~/logic/state/contact';
import {useHistory} from 'react-router-dom';
import {Portal} from './Portal';
import {getRelativePosition} from '~/logic/lib/relativePosition';

export const OVERLAY_HEIGHT = 250;
const FixedOverlay = styled(Col)`
  position: fixed;
  -webkit-transition: all 0.2s ease-out;
  -moz-transition: all 0.2s ease-out;
  -o-transition: all 0.2s ease-out;
  transition: all 0.2s ease-out;
`;

type ProfileOverlayProps = BoxProps & {
  ship: string;
  api: any;
};

const ProfileOverlay = (props: ProfileOverlayProps) => {
  const {
    ship,
    api,
    children,
    ...rest
  } = props;
  const [open, _setOpen] = useState(false);
  const [coords, setCoords] = useState({});
  const [visible, setVisible] = useState(false);
  const history = useHistory();
  const outerRef = useRef<HTMLElement | null>(null);
  const innerRef = useRef<HTMLElement | null>(null);
  const hideAvatars = useSettingsState(state => state.calm.hideAvatars);
  const hideNicknames = useSettingsState(state => state.calm.hideNicknames);
  const isOwn = useMemo(() => `~${window.ship}` === ship, [ship])

  const contact = useContact(ship)
  const color = uxToHex(contact?.color ?? '0x0');
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
    const updateCoords = _.throttle(_updateCoords, 50);
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
      <Box size={60} backgroundColor={color}>
        <Center height={60}>
          <Sigil ship={ship} size={32} color={color} />
        </Center>
      </Box>
    );

  return (
    <Box ref={outerRef} {...rest} onClick={setOpen} cursor="pointer">
      <VisibilitySensor onChange={setVisible}>
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
        fontSize='0'
        height='250px'
        width='250px'
        padding={3}
        justifyContent='center'
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
              title={contact?.status ? contact.status : ''}
            >
              {contact?.status ? contact.status : ''}
            </RichText>
          )}
        </Col>
      </FixedOverlay>
    </Portal>
    )}
  </Box>
  );
};

export default ProfileOverlay;
