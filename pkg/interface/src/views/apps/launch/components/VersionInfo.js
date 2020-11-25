import React from 'react';
import { Box, Text, Icon } from '@tlon/indigo-react';

export default class VersionInfo extends React.Component {
  componentDidMount() {
    this.props.api.local.getRuntimeLag();
  }

  render() {
    const { props } = this;
    return (
      <Box
        position="absolute"
        fontFamily="mono"
        left="0"
        bottom="0"
        color="gray"
        bg="white"
        ml={3}
        mb={3}
        borderRadius={2}
        fontSize={0}
        p={2}
      >
        {props.baseHash}
        { props.runtimeLag && (
          <Icon
            color="yellow"
            fill="rgba(0,0,0,0)"
            icon="Circle"
          />
        )}
      </Box>
    );
  }
}
