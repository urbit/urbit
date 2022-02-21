import { BaseImage, Box, BoxProps, Button, Center, Col, Icon, IconIndex, LoadingSpinner, Row, Text } from '@tlon/indigo-react';
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
import api from '~/logic/api';
import useDocketState from '~/logic/state/docket';
import { PALS_APP, PALS_HOST } from '~/logic/constants/install';
import { ModalOverlay } from './ModalOverlay';

export const OVERLAY_HEIGHT = 250;
const FixedOverlay = styled(Col)`
  position: fixed;
  -webkit-transition: all 0.1s ease-out;
  -moz-transition: all 0.1s ease-out;
  -o-transition: all 0.1s ease-out;
  transition: all 0.1s ease-out;
`;

const ActionRow = styled(Row)`
  padding: 4px;
  width: 80%;
  cursor: pointer;
  &:hover {
    background-color: ${p => p.theme.colors.washedGray};
  }
`;

const PalsInfo = () => {
  const { addAlly, requestTreaty, installDocket } = useDocketState();
  const [hasPalsApp, setHasPalsApp] = useState(false);
  const [isInstalling, setIsInstalling] = useState(false);
  const [showInstallModal, setShowInstallModal] = useState(false);

  useEffect(() => {
    const getPalsData = async () => {
      try {
        const data = await api.scry({
          app: 'pals',
          path: '/'
        });
        setHasPalsApp(true);
        console.log('PALS DATA:', data)
      } catch (err) {
        // TODO: figure out which one means app isn't installed
      }
    };

    getPalsData();
  }, []);
  const isMutual = false;
  const isLeech = false;
  const isTarget = false;

  const handleSendRequest = useCallback(() => {
    if (hasPalsApp) {
      // send pals request, also on line below
    } else {
      setShowInstallModal(true);
    }
  }, [hasPalsApp]);

  const handleInstall = useCallback(async () => {
    setIsInstalling(true);
    setShowInstallModal(false);
    try {
      await addAlly(PALS_HOST);
      await requestTreaty(PALS_HOST, PALS_APP);
      await installDocket(PALS_HOST, PALS_APP);
      setHasPalsApp(true);
    } catch (err) {
      console.warn('PALS INSTALL ERROR:', err);
    } finally {
      setIsInstalling(false);
    }
  }, [setIsInstalling, setShowInstallModal, addAlly, requestTreaty, installDocket, setHasPalsApp]);

  const getActionProps = () : { icon: keyof IconIndex; text: string | Element; onClick?: () => void } => {
    if (isMutual) {
      return { icon: 'Users', text: 'You are pals' };
    } else if (isLeech) {
      return { icon: 'Clock', text: 'Pals request pending', onClick: () => null };
    } else if (isTarget) {
      return { icon: 'CheckmarkBold', text: 'Accept pals request', onClick: () => null };
    }

    // TODO: if %pals isn't installed, show a modal with the option to install %pals
    return { icon: 'CreateGroup', text: 'Send pals request', onClick: handleSendRequest };
  };

  if (showInstallModal) {
    return (
      <ModalOverlay
        bg="transparent"
        height="100%"
        width="100%"
        display="flex"
        flexDirection="column"
        justifyContent="center"
        alignItems="center"
        dismiss={() => setShowInstallModal(false)}
      >
        <Box backgroundColor="white" p={4} borderRadius={3} display="flex" flexDirection="column" alignItems="center">
          <Text>You do not have <Text mono>%pals</Text> installed,<br /> would you like to install it?</Text>
          <Row mt={3}>
            <Button color="white" backgroundColor="black" onClick={() => setShowInstallModal(false)} >Cancel</Button>
            <Button ml={2} onClick={handleInstall}>Install</Button>
          </Row>
        </Box>
      </ModalOverlay>
    );
  } else if (isInstalling) {
    return (
      <ActionRow width="80%">
        <LoadingSpinner />
        <Text ml={2}>Installing...</Text>
      </ActionRow>
    );
  }

  const { icon, text, onClick } = getActionProps();

  return (
    <ActionRow width="80%" onClick={onClick}>
      <Icon icon={icon} size={16} mr={2} />
      <Text>{text}</Text>
    </ActionRow>
  );
};

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
        height='250px'
        width='250px'
        padding={3}
        alignItems='center'
      >
        <Box
          height='72px'
          cursor='pointer'
          onClick={() => history.push(`/~profile/~${ship}`)}
          overflow='hidden'
          borderRadius={2}
        >
          {img}
        </Box>
        <Col width="100%" alignItems="center">
          <Row>
            <Text
              fontWeight='600'
              mono={!showNickname}
              textOverflow='ellipsis'
              overflow='hidden'
              whiteSpace='pre'
              marginBottom={0}
              cursor='pointer'
              display={didCopy ? 'none' : 'block'}
              onClick={doCopy}
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
              width='100%'
              minWidth={0}
              textOverflow='ellipsis'
              overflow='hidden'
              whiteSpace='pre'
              mb={0}
              disableRemoteContent
              gray
              title={contact?.status ? contact.status : ''}
            >
              {contact?.status ? contact.status : ''}
            </RichText>
          )}
        </Col>
        {!isOwn && (
          <>
            <ActionRow mt={2} onClick={() => history.push(`/~landscape/messages/dm/~${ship}`)}>
              <Icon icon='Chat' size={16} mr={2} />
              <Text>Message</Text>
            </ActionRow>
            {/* <PalsInfo /> */}
          </>
        )}
      </FixedOverlay>
    </Portal>
    )}
  </Box>
  );
};

export default ProfileOverlay;
