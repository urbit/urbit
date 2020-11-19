import React from 'react';

import { Row, Box, Text, Icon, Button } from '@tlon/indigo-react';
import ReconnectButton from './ReconnectButton';
import { StatusBarItem } from './StatusBarItem';
import { Sigil } from '~/logic/lib/sigil';

const StatusBar = (props) => {
  const invites = [].concat(...Object.values(props.invites).map(obj => Object.values(obj)));
  const metaKey = (window.navigator.platform.includes('Mac')) ? '⌘' : 'Ctrl+';

  return (
    <Box
      display='grid'
      width="100%"
      gridTemplateRows="30px"
      gridTemplateColumns="3fr 1fr"
      py='3'
      px='3'
      pb='3'
      >
      <Row collapse>
      <Button borderColor='washedGray' mr='2' px='2' onClick={() => props.history.push('/')} {...props}>
        <Icon icon='Spaces' color='black'/>
      </Button>

        <StatusBarItem mr={2} onClick={() => props.api.local.setOmnibox()}>
        { !props.doNotDisturb && (props.notificationsCount > 0 || invites.length > 0) &&
          (<Box display="block" right="-8px" top="-8px" position="absolute" >
            <Icon color="blue" icon="Bullet" />
           </Box>
        )}
        <Icon icon='LeapArrow'/>
          <Text ml={2} color='black'>
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
      <Row justifyContent="flex-end" collapse>
        <StatusBarItem px={'2'} flexShrink='0' onClick={() => props.history.push('/~profile')}>
          <Sigil ship={props.ship} size={16} color='black' classes='mix-blend-diff' icon />
          <Text ml={2} display={["none", "inline"]} fontFamily="mono">~{props.ship}</Text>
        </StatusBarItem>
      </Row>
    </Box>
  );
};

export default StatusBar;
