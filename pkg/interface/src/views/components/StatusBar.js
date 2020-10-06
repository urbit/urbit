import React from 'react';

import { useLocation } from 'react-router-dom';
import { Row, Box, Text, Icon } from '@tlon/indigo-react';
import ReconnectButton from './ReconnectButton';
import { StatusBarItem } from './StatusBarItem';
import { Sigil } from '~/logic/lib/sigil';


const StatusBar = (props) => {

  const location = useLocation();

  const display = (!window.location.href.includes('popout/'))
      ? 'grid' : 'none';

  const invites = (props.invites && props.invites['/contacts'])
    ? props.invites['/contacts']
    : {};


  const metaKey = (window.navigator.platform.includes('Mac')) ? '⌘' : 'Ctrl+';

  return (
    <Box
      display={display}
      width="100%"
      gridTemplateRows="30px"
      gridTemplateColumns="3fr 1fr"
      py={2}
      px={3}
      >
      <Row collapse>
          <StatusBarItem mr={2} onClick={() => props.history.push('/')}>
          <Icon icon='Home' color='transparent' stroke='black' />
          </StatusBarItem>
        <StatusBarItem mr={2} onClick={() => props.api.local.setOmnibox()}>
        <Icon icon='LeapArrow'/>
          <Text ml={2} color='black'>
            Leap
          </Text>
          <Text display={['none', 'inline']} ml={4} color='gray'>
            {metaKey}/
          </Text>
        </StatusBarItem>
        <StatusBarItem
          onClick={() => props.history.push('/~groups')}
          badge={Object.keys(invites).length > 0}>
          <Icon icon='Groups' color='transparent' stroke='black'/>
          <Text display={["none", "inline"]} ml={2}>Groups</Text>
        </StatusBarItem>
        <ReconnectButton
          connection={props.connection}
          subscription={props.subscription}
        />
      </Row>
      <Row justifyContent="flex-end" collapse>
        <StatusBarItem onClick={() => props.history.push('/~profile')}>
          <Sigil ship={props.ship} size={24} color={"#000000"} classes="dib mix-blend-diff" />
          <Text ml={2} display={["none", "inline"]} fontFamily="mono">~{props.ship}</Text>
        </StatusBarItem>
      </Row>
    </Box>
  );
};

export default StatusBar;
