import React, { Component } from 'react';
import { Box, Text, Button } from '@tlon/indigo-react';

export class ResubscribeElement extends Component {
  onClickResubscribe() {
    this.props.api.chat.addSynced(
      this.props.host,
      this.props.station,
      true);
  }

  render() {
    const { props } = this;
    if (props.isChatUnsynced) {
      return (
        <Box p='3' m='3' border='1px solid' borderColor='yellow' backgroundColor='lightYellow'>
          <Text lineHeight='tall' display='block'>
            Your ship has been disconnected from the chat's host.
            This may be due to a bad connection, going offline, lack of permission,
            or an over-the-air update.
          </Text>
          <Button
            primary
            mt='3'
            onClick={this.onClickResubscribe.bind(this)}
          >
            Reconnect to this chat
          </Button>
        </Box>
      );
    } else {
      return null;
    }
  }
}
