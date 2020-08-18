import React from 'react';
import { Row, Box, Text, Icon } from '@tlon/indigo-react';
import ReconnectButton from './ReconnectButton';

const StatusBar = (props) => {
  const invites = (props.invites && props.invites['/contacts'])
    ? props.invites['/contacts']
    : {};

  const Notification = (Object.keys(invites).length > 0)
    ?  <Icon size="22px" icon="Bullet"
          fill="blue" position="absolute"
          top={'-8px'} right={'7px'}
       />
    : null;

  const metaKey = (window.navigator.platform.includes('Mac')) ? '⌘' : 'Ctrl+';

  const mobile = /Android|webOS|iPhone|iPad|iPod|BlackBerry/i.test(
    navigator.userAgent
  );

  return (
    <Row
    height="45px"
    backgroundColor="white"
    width="100%"
    justifyContent="space-between"
    pt="10px"
    display={(window.location.href.includes('popout/') ? 'none' : 'flex')}>
      <Box pl={3} display="inline-block">
          <Box
            style={{ cursor: 'pointer' }}
            display='inline-block'
            borderRadius={2}
            verticalAlign="middle"
            lineHeight="0"
            color='washedGray'
            border={1}
            py="6px"
            px={2}
            mr={2}
            onClick={() => props.history.push('/')}>
            <img
              className='invert-d'
              src='/~landscape/img/icon-home.png'
              height='11'
              width='11'
            />
          </Box>
        <Box
          border={1}
          borderRadius={2}
          color='washedGray'
          display='inline-block'
          verticalAlign='middle'
          lineHeight="0"
          style={{ cursor: 'pointer' }}
          py={1}
          px={2}
          onClick={() => props.api.local.setOmnibox()}>
          <Text display='inline-block' style={{ transform: 'rotate(180deg)' }}>
            ↩
          </Text>
          <Text ml={2} color='black'>
            Leap
          </Text>
          <Text display={mobile ? 'none' : 'inline-block'} ml={4} color='gray'>
            {metaKey}/
          </Text>
        </Box>
        <ReconnectButton
          connection={props.connection}
          subscription={props.subscription}
        />
      </Box>
      <Box position="relative" pr={3} display="inline-block">
        <Box
          style={{ cursor: 'pointer' }}
          display='inline-block'
          borderRadius={2}
          color='washedGray'
          verticalAlign="middle"
          lineHeight='0'
          border={1}
          px={2}
          py={1}
          onClick={() => props.history.push('/~groups')}>
          <img
            className='invert-d v-mid mr1'
            src='/~landscape/img/groups.png'
            height='15'
            width='15'
          />
          {Notification}
          <Text ml={1}>Groups</Text>
        </Box>
      </Box>
    </Row>
  );
};

export default StatusBar;
