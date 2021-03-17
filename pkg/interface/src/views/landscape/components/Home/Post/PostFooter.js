import React from 'react';
import { Box, Col, Row, Text, Icon } from '@tlon/indigo-react';


export function PostFooter(props) {
  const { replyCount, toggleReplyMode } = props;

  return (
    <Row mt={2} justify-content="flex-start">
      <Row cursor="pointer" onClick={toggleReplyMode}>
        <Icon icon="Chat" />
        { replyCount > 0 ? (
          <Text pl="1" gray>{replyCount}</Text>
        ) : null }
      </Row>
    </Row>
  );
}

