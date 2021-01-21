import React from 'react';

import { Row, Box, Text, Icon, Button } from '@tlon/indigo-react';
import ReconnectButton from './ReconnectButton';
import { StatusBarItem } from './StatusBarItem';
import { Sigil } from '~/logic/lib/sigil';
import useLocalState from '~/logic/state/local';

const StatusBar = (props) => {
  const invites = [].concat(...Object.values(props.invites).map(obj => Object.values(obj)));
  const metaKey = (window.navigator.platform.includes('Mac')) ? '⌘' : 'Ctrl+';
  const toggleOmnibox = useLocalState(state => state.toggleOmnibox);

  const color = !!props.ourContact ? props.ourContact.color : 'black';
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
        <StatusBarItem mr={2} onClick={() => toggleOmnibox()}>
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
        <StatusBarItem
          mr='2'
          backgroundColor='yellow'
          display={process.env.LANDSCAPE_STREAM === 'development' ? 'flex' : 'none'}
          justifyContent="flex-end"
          flexShrink='0'
          onClick={() => window.open(
            'https://github.com/urbit/landscape/issues/new' +
            '?assignees=&labels=development-stream&title=&' +
            `body=commit:%20${process.env.LANDSCAPE_SHORTHASH}`
            )}
          >
          <Text color='#000000'>Submit <Text color='#000000' display={['none', 'inline']}>an</Text> issue</Text>
        </StatusBarItem>
        <StatusBarItem mr={2} px={'2'} flexShrink='0' onClick={() => props.history.push('/~settings')}>
          <Icon icon='Gear' color='black'/>
        </StatusBarItem>
        <StatusBarItem
          px={'2'}
          flexShrink='0'
          onClick={() => props.history.push('/~profile/' + window.ship)}>
          <Sigil ship={props.ship} size={16} color={color} classes='mix-blend-diff' icon />
        </StatusBarItem>
      </Row>
    </Box>
  );
};

export default StatusBar;
