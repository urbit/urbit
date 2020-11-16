import React, { Component } from 'react';
import { Invite } from '~/types/invite-update';
import { Text, Box, Button, Row } from '@tlon/indigo-react';
import { cite } from '~/logic/lib/util';

export class InviteItem extends Component<{invite: Invite, onAccept: Function, onDecline: Function}, {}> {
  render() {
    const { props } = this;

    return (
      <Box width='100%' p='4' mb='4' borderBottom='1px solid lightGray' position='sticky' style={{ top: 0 }}>
        <Box width='100%' verticalAlign='middle'>
          <Text display='block' pb='2' gray><Text mono>{cite(props.invite.resource.ship)}</Text> invited you to <Text fontWeight='500'>{props.invite.resource.name}</Text></Text>
        </Box>
        <Row>
        <Button
          cursor='pointer'
          primary
          mt='4'
          display='inline-block'
          onClick={this.props.onAccept.bind(this)}
        >
          Accept
        </Button>
        <Button
          display='inline-block'
          cursor='pointer'
          ml='4'
          mt='4'
          onClick={this.props.onDecline.bind(this)}
        >
          Decline
        </Button>
        </Row>
      </Box>
    );
  }
}

export default InviteItem;
