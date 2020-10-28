import React from 'react';

import { useLocation } from 'react-router-dom';
import { Row, Box, Text, Icon } from '@tlon/indigo-react';
import ReconnectButton from './ReconnectButton';
import { StatusBarItem } from './StatusBarItem';
import { Sigil } from '~/logic/lib/sigil';


const StatusBar = (props) => {

  const metaKey = (window.navigator.platform.includes('Mac')) ? '⌘' : 'Ctrl+';

  return (
    <Box
      display='grid'
      width="100%"
      gridTemplateRows="30px"
      gridTemplateColumns="3fr 1fr"
      py={[2,3]}
      px={[2,3]}
      >
      <Row collapse>
          <StatusBarItem mr={2} onClick={() => props.history.push('/')}>
          <Icon icon='Home' color='black'/>
          </StatusBarItem>
        <StatusBarItem mr={2} onClick={() => props.api.local.setOmnibox()}>
        { !props.doNotDisturb && props.notificationsCount > 0 && 
          (<Box display="block" right="-8px" top="-8px" position="absolute" >
            <Icon color="blue" icon="Bullet" />
           </Box>
        )}
        <Icon icon='LeapArrow'/>
          <Text ml={2} color='black'>
            Leap
          </Text>
          <Text display={['none', 'inline']} ml={4} color='gray'>
            {metaKey}/
          </Text>
        </StatusBarItem>
        <ReconnectButton
          connection={props.connection}
          subscription={props.subscription}
        />
      </Row>
      <Row justifyContent="flex-end" collapse>
        <StatusBarItem onClick={() => props.history.push('/~profile')}>
          <Sigil ship={props.ship} size={16} color='black' classes='mix-blend-diff' icon />
          <Text ml={2} display={["none", "inline"]} fontFamily="mono">~{props.ship}</Text>
        </StatusBarItem>
      </Row>
    </Box>
  );
};

export default StatusBar;
