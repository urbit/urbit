import React from 'react';
import { Box, Col, Row, Text, Icon } from '@tlon/indigo-react';


export function PostFooter(props) {
  const navigateToReplies = () => {
    console.log('TODO');
  };
  return (
    <Row mt={2} justify-content="flex-start">
      <Row cursor="pointer" onClick={navigateToReplies}>
        <Icon icon="Chat" />
        { props.replyCount > 0 ? (
          <Text pl="1" gray>{props.replyCount}</Text>
        ) : null }
      </Row>
    </Row>
  );
}

