import { BaseImage, Box, Button, Col, H3, Icon, Image, Row, Text } from '@tlon/indigo-react';
import React, { useCallback } from 'react';
import { Link } from 'react-router-dom';
import { Sigil } from '~/logic/lib/sigil';
import { uxToHex } from '~/logic/lib/util';
import useContactState from '~/logic/state/contact';
import useLocalState, { selectLocalState } from '~/logic/state/local';
import useSettingsState, { selectCalmState } from '~/logic/state/settings';
import { Dropdown } from './Dropdown';
import { ProfileStatus } from './ProfileStatus';
import ReconnectButton from './ReconnectButton';
import { StatusBarItem } from './StatusBarItem';
import useHarkState from '~/logic/state/hark';
import useMetadataState from '~/logic/state/metadata';
import UqbarLogo from '~/assets/img/uqbar-logo.png';
import { useDmUnreads } from '~/logic/lib/useDmUnreads';
import { bootstrapApi } from '~/logic/api/bootstrap';

const localSel = selectLocalState(['toggleOmnibox']);

const StatusBar = (props) => {
  const { ship } = props;
  const ourContact = useContactState(state => state.contacts[`~${ship}`]);
  const metaKey = window.navigator.platform.includes('Mac') ? '⌘' : 'Ctrl+';
  const { toggleOmnibox } = useLocalState(localSel);
  const { hideAvatars } = useSettingsState(selectCalmState);
  const { notificationsCount } = useHarkState();
  const { unreadDmCount } = useDmUnreads();
  const groups = useMetadataState(s => s.associations.groups);
  const isHome = props.location.pathname === '/';

  const refreshConnections = useCallback(() => {
    bootstrapApi(true);
  }, []);

  let title = `~${window.ship}`;
  if (props.location.pathname.includes('~landscape')) {
    const [, , , groupShip, groupName] = props.location.pathname.split('/');
    title = groups[`/ship/${groupShip}/${groupName}`]?.metadata?.title;
  }

  const color = ourContact ? `#${uxToHex(ourContact.color)}` : '#000';
  const xPadding = !hideAvatars && ourContact?.avatar ? '0' : '2';
  const bgColor = !hideAvatars && ourContact?.avatar ? '' : color;
  const profileImage =
    !hideAvatars && ourContact?.avatar ? (
      <BaseImage
        src={ourContact.avatar}
        borderRadius={2}
        width='32px'
        height='32px'
        style={{ objectFit: 'cover' }}
      />
    ) : (
      <Sigil ship={ship} size={16} color={color} icon />
    );

  return (
    <Box
      display='grid'
      width='100%'
      gridTemplateRows='30px'
      gridTemplateColumns='1fr 1fr 1fr'
      py={3}
      px={3}
      pb={3}
    >
      <Row>
        <Button
          as={Link}
          to="/"
          width='32px'
          borderColor='lightGray'
          mr={2}
          px={1}
          {...props}
        >
          <Image
            referrerPolicy="no-referrer"
            src={UqbarLogo}
            height="20px"
            width="20px"
            onError={console.warn}
          />
          {/* <Icon icon='Dashboard' color='black' /> */}
        </Button>
        <StatusBarItem position="relative" mr={2} onClick={() => toggleOmnibox()}>
          <Icon icon='Menu' />
          <Text display={['none', 'inline']} ml={2} color='gray'>
            {metaKey}/
          </Text>
          { notificationsCount > 0 && (
            <Box position="absolute" right="-8px" top="-8px">
              <Icon icon="Bullet" color="blue" />
            </Box>
          )}
        </StatusBarItem>
        <ReconnectButton />
      </Row>
      <H3
        textAlign="center"
        verticalAlign="middle"
        lineHeight="28px"
        textOverflow="ellipsis"
        whiteSpace="nowrap"
        overflow="hidden"
        mono={isHome}
      >
        {title}
      </H3>
      <Row justifyContent='flex-end'>
        {process.env.LANDSCAPE_STREAM === 'development' && <StatusBarItem
          width='32px'
          mr={2}
          backgroundColor='yellow'
          justifyContent='center'
          flexShrink={0}
          onClick={() =>
            window.open(
              'https://github.com/willbach/urbit/issues/new' +
                '?assignees=&labels=development-stream&title=&' +
                `body=commit:%20urbit/urbit@${process.env.LANDSCAPE_SHORTHASH}`
            )
          }
        >
          <Icon icon="Bug" color="#000000" />
        </StatusBarItem>}
        <StatusBarItem
          as={Button}
          width='32px'
          mr={2}
          onClick={refreshConnections}
        >
          <Icon icon='ArrowRefresh' />
        </StatusBarItem>
        <StatusBarItem
          as={Link}
          to="/~landscape/messages"
          width='32px'
          mr={2}
        >
          <Icon icon='Messages' />
          { unreadDmCount > 0 && (
            <Box position="absolute" right="-8px" top="-8px">
              <Icon icon="Bullet" color="blue" />
            </Box>
          )}
        </StatusBarItem>
        <Dropdown
          dropWidth='250px'
          width='auto'
          alignY='top'
          alignX='right'
          flexShrink={0}
          offsetY={-48}
          options={
            <Col
              py={2}
              backgroundColor='white'
              color='washedGray'
              border={1}
              borderRadius={2}
              borderColor='lightGray'
              boxShadow='0px 0px 0px 3px'
            >
              <Row
                as={Link}
                to={`/~profile/~${ship}`}
                color='black'
                cursor='pointer'
                fontSize={1}
                fontWeight='500'
                px={3}
                py={2}
              >
                View Profile
              </Row>
              <Row
                as={Link}
                to="/~settings"
                color='black'
                cursor='pointer'
                fontSize={1}
                fontWeight='500'
                px={3}
                py={2}
              >
                System Preferences
              </Row>
              <Row px={3} pt={2} pb={1} flexDirection='column'>
                <Text color='gray' fontWeight='500' mb={1}>
                  Set Status:
                </Text>
                <ProfileStatus
                  contact={ourContact}
                  ship={`~${ship}`}
                />
              </Row>
            </Col>
          }
        >
          <StatusBarItem
            px={xPadding}
            width='32px'
            flexShrink={0}
            border={0}
            backgroundColor={bgColor}
          >
            {profileImage}
          </StatusBarItem>
        </Dropdown>
      </Row>
    </Box>
  );
};

export default StatusBar;
