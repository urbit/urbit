import React from 'react';
import { useLocation } from 'react-router-dom';
import { Box, Text, Icon } from '@tlon/indigo-react';
import ReconnectButton from './ReconnectButton';

const StatusBar = (props) => {
  const location = useLocation();
  const atHome = Boolean(location.pathname === '/');

  const display = (!window.location.href.includes('popout/'))
      ? 'db' : 'dn';

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
    <div
      className={
        'bg-white bg-gray0-d w-100 justify-between relative tc pt3 ' + display
      }
      style={{ height: 45 }}
    >
      <div className='absolute left-0 pl4' style={{ top: 10 }}>
        {atHome ? null : (
          <Box
            style={{ cursor: 'pointer' }}
            display='inline-block'
            borderRadius={2}
            color='washedGray'
            border={1}
            py={1}
            px={2}
            mr={2}
            onClick={() => props.history.push('/')}
          >
            <img
              className='invert-d'
              src='/~landscape/img/icon-home.png'
              height='12'
              width='12'
            />
          </Box>
        )}
        <Box
          border={1}
          borderRadius={2}
          color='washedGray'
          display='inline-block'
          style={{ cursor: 'pointer' }}
          lineHeight="min"
          py={1}
          px={2}
          onClick={() => props.api.local.setOmnibox()}
        >
          <Text display='inline-block' style={{ transform: 'rotate(180deg)' }}>
            ↩
          </Text>
          <Text ml={2} color='black'>
            Leap
          </Text>
          <Text display={mobile ? 'none' : 'inline-block'} ml={4} color='gray'>
            {metaKey}L
          </Text>
        </Box>
        <ReconnectButton
        connection={props.connection}
        subscription={props.subscription}
        />
      </div>
      <div className='fl absolute relative right-0 pr4' style={{ top: 10 }}>
        <Box
          style={{ cursor: 'pointer' }}
          display='inline-block'
          borderRadius={2}
          color='washedGray'
          border={1}
          px={2}
          py={1}
          onClick={() => props.history.push('/~groups')}
        >
          <img
            className='invert-d v-mid'
            src='/~landscape/img/groups.png'
            height='16'
            width='16'
          />
          {Notification}
          <Text ml={1}>Groups</Text>
        </Box>
      </div>
    </div>
  );
};

export default StatusBar;
