import React, { Component } from 'react';
import { Sigil } from '~/logic/lib/sigil';
import { cite } from '~/logic/lib/util';
import moment from 'moment';
import { Box, Text, Row } from '@tlon/indigo-react';
import RichText from '~/views/components/RichText';

export const CommentItem = (props) => {
  const content = props.post.contents[0].text;
  const timeSent = 
    moment.unix(props.post['time-sent'] / 1000).format('hh:mm a');

  const showAvatar = props.avatar && !props.hideAvatars;
  const showNickname = props.nickname && !props.hideNicknames;
  const img = showAvatar
    ? <img src={props.avatar} height={36} width={36} className="dib" />
    : <Sigil
        ship={`~${props.post.author}`}
        size={36}
        color={`#${props.color}`}
        classes={(!!props.member ? 'mix-blend-diff' : '')}
      />;

  return (
    <Box width="100%" py={3} opacity={props.pending ? '0.6' : '1'}>
      <Row backgroundColor='white'>
        {img}
        <Row fontSize={0} alignItems="center" ml={2}>
          <Text mono={!props.hasNickname} title={props.post.author}>
            {showNickname ? props.nickname : cite(props.post.author)}
          </Text>
          <Text gray ml={2}>{timeSent}</Text>
        </Row>
      </Row>
      <Row>
        <Text display="block" py={3} fontSize={1}>
          <RichText remoteContentPolicy={props.remoteContentPolicy}>
            {content}
          </RichText>
        </Text>
      </Row>
    </Box>
  );
}

