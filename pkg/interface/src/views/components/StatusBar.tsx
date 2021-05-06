import {
  BaseImage, Box,

  Button, Col,

  Icon, Row,

  Text
} from '@tlon/indigo-react';
import React, { useRef } from 'react';
import { useHistory } from 'react-router-dom';
import { Sigil } from '~/logic/lib/sigil';
import { uxToHex } from '~/logic/lib/util';
import useContactState from '~/logic/state/contact';
import useHarkState from '~/logic/state/hark';
import useInviteState from '~/logic/state/invite';
import useLocalState, { selectLocalState } from '~/logic/state/local';
import useSettingsState, { selectCalmState } from '~/logic/state/settings';
import { Dropdown } from './Dropdown';
import { ProfileStatus } from './ProfileStatus';
import ReconnectButton from './ReconnectButton';
import { StatusBarItem } from './StatusBarItem';
import { useTutorialModal } from './useTutorialModal';

const localSel = selectLocalState(['toggleOmnibox']);

const StatusBar = (props) => {
  const { api, ship } = props;
  const history = useHistory();
  const ourContact = useContactState(state => state.contacts[`~${ship}`]);
  const notificationsCount = useHarkState(state => state.notificationsCount);
  const doNotDisturb = useHarkState(state => state.doNotDisturb);
  const inviteState = useInviteState(state => state.invites);
  const invites = [].concat(
    ...Object.values(inviteState).map(obj => Object.values(obj))
  );
  const metaKey = window.navigator.platform.includes('Mac') ? '⌘' : 'Ctrl+';
  const { toggleOmnibox } = useLocalState(localSel);
  const { hideAvatars } = useSettingsState(selectCalmState);

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

  const anchorRef = useRef(null);

  const leapHighlight = useTutorialModal('leap', true, anchorRef);

  const floatLeap =
    leapHighlight && window.matchMedia('(max-width: 550px)').matches;

  return (
    <Box
      display='grid'
      width='100%'
      gridTemplateRows='30px'
      gridTemplateColumns='3fr 1fr'
      py={3}
      px={3}
      pb={3}
    >
      <Row collapse>
        <Button
          width='32px'
          borderColor='lightGray'
          mr={2}
          px={2}
          onClick={() => history.push('/')}
          {...props}
        >
          <Icon icon='Dashboard' color='black' />
        </Button>
        <StatusBarItem float={floatLeap} mr={2} onClick={() => toggleOmnibox()}>
          {!doNotDisturb && (notificationsCount > 0 || invites.length > 0) && (
            <Box display='block' right='-8px' top='-8px' position='absolute'>
              <Icon color='blue' icon='Bullet' />
            </Box>
          )}
          <Icon icon='LeapArrow' />
          <Text ref={anchorRef} ml={2} color='black'>
            Leap
          </Text>
          <Text display={['none', 'inline']} ml={2} color='gray'>
            {metaKey}/
          </Text>
        </StatusBarItem>
        <ReconnectButton
          connection={props.connection}
          subscription={props.subscription}
        />
      </Row>
      <Row justifyContent='flex-end' collapse>
        <StatusBarItem
          mr={2}
          backgroundColor='yellow'
          display={
            process.env.LANDSCAPE_STREAM === 'development' ? 'flex' : 'none'
          }
          justifyContent='flex-end'
          flexShrink={0}
          onClick={() =>
            window.open(
              'https://github.com/urbit/landscape/issues/new' +
                '?assignees=&labels=development-stream&title=&' +
                `body=commit:%20urbit/urbit@${process.env.LANDSCAPE_SHORTHASH}`
            )
          }
        >
        <Icon icon="Bug" color="#000000" />
        </StatusBarItem>
        <StatusBarItem
          width='32px'
          mr={2}
          onClick={() => props.history.push('/~landscape/messages')}
        >
          <Icon icon='Messages' />
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
                color='black'
                cursor='pointer'
                fontSize={1}
                fontWeight='500'
                px={3}
                py={2}
                onClick={() => history.push(`/~profile/~${ship}`)}
              >
                View Profile
              </Row>
              <Row
                color='black'
                cursor='pointer'
                fontSize={1}
                fontWeight='500'
                px={3}
                py={2}
                onClick={() => history.push('/~settings')}
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
                  api={api}
                />
              </Row>
            </Col>
          }
        >
          <StatusBarItem
            px={xPadding}
            width='32px'
            flexShrink={0}
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
