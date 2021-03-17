import React from 'react';
import { Box, Col, Row, Text, Icon } from '@tlon/indigo-react';


export function PostFooter(props) {
  const navigateToReplies = () => {
    console.log('TODO');
  };
  return (
    <Row justify-content="flex-start">
      <Box cursor="pointer" onClick={navigateToReplies}>
        <Icon icon="Chat" size="12" />
        { props.replyCount > 0 ? (
          <Text pl="1">{props.replyCount}</Text>
        ) : null }
      </Box>
    </Row>
  );
}

