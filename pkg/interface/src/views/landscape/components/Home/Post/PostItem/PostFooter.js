import moment from 'moment';
import React from 'react';
import { Box, Col, Row, Text, Icon } from '@tlon/indigo-react';
import Timestamp from '~/views/components/Timestamp';


export function PostFooter(props) {
  const {
    replyCount,
    toggleReplyMode,
    showTimestamp,
    isParent,
    timeSent,
    canComment
  } = props;
  const stamp = moment(timeSent);
  const mt = showTimestamp && isParent ? "2" : "0";

  const replyText = replyCount === 1 ? ' reply' : ' replies';

  return (
    <Row
      mt={mt}
      justify-content="flex-start"
      width="100%"
      opacity={canComment ? 1 : 0}>
      <Col width="100%">
        { showTimestamp && (
          <Row
            width="100%"
            borderBottom={1}
            borderBottomColor="lightGray" pb="2">
            { showTimestamp ? (
              <Text ml="2">{replyCount}{replyText}</Text>
            ) : null }
            <Timestamp
              stamp={stamp}
              fontSize={1}
              time={true}
              date={true}
              ml="2"
              dateNotRelative={true}
              color='gray'
            />
          </Row>
        )}
        <Row height={showTimestamp ? "32px" : ""}
          alignItems="center"
          cursor="pointer"
          pl="2"
          pr="2"
          mb={showTimestamp ? "" : "2"}
          onClick={(e) => {
            e.stopPropagation();
            toggleReplyMode();
          }}>
          <Icon icon="Chat" />
          { replyCount > 0 && !showTimestamp ? (
            <Text pl="1" gray>{replyCount}</Text>
          ) : null }
        </Row>
      </Col>
    </Row>
  );
}

