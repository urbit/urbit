import React from 'react';
import { cite } from '~/logic/lib/util';
import RemoteContent from '~/views/components/RemoteContent';

import { Box, Col, Anchor, Text } from '@tlon/indigo-react';

import moment from 'moment';

const URLparser = new RegExp(
    /((?:([\w\d\.-]+)\:\/\/?){1}(?:(www)\.?){0,1}(((?:[\w\d-]+\.)*)([\w\d-]+\.[\w\d]+))){1}(?:\:(\d+)){0,1}((\/(?:(?:[^\/\s\?]+\/)*))(?:([^\?\/\s#]+?(?:.[^\?\s]+){0,1}){0,1}(?:\?([^\s#]+)){0,1})){0,1}(?:#([^#\s]+)){0,1}/
);

export const LinkPreview = (props) => {
  const showNickname = props.nickname && !props.hideNicknames;
  const author = props.post.author;
  const title = props.post.contents[0].text;
  const url = props.post.contents[1].url;
  const hostname = URLparser.exec(url) ? URLparser.exec(url)[4] : null;

  const timeSent =
    moment.unix(props.post['time-sent'] / 1000).format('hh:mm a');

  const embed = (
    <RemoteContent
      unfold={true}
      renderUrl={false}
      url={url}
      remoteContentPolicy={props.remoteContentPolicy}
      className="mw-100"
    />
  );

  return (
    <Box pb='6' width='100%'>
      <Box width='100%' textAlign='center'>{embed}</Box>
      <Col flex='1 1 auto' minWidth='0' minHeight='0' pt='6'>
        <Anchor href={url}
          lineHeight="tall"
          display='flex'
          style={{ textDecoration: 'none' }}
          width='100%'
          target="_blank"
          rel="noopener noreferrer">
          <Text
            display='inline-block'
            overflow='hidden'
            style={{ textOverflow: 'ellipsis', whiteSpace: 'pre' }}
          >
            {title}
          </Text>
          <Text ml="2" color="gray" display='inline-block' flexShrink='0'>{hostname} ↗</Text>
        </Anchor>
        <Box width='100%' pt='1'>
          <Text fontSize='0' pr='2' display='inline-block' mono={!showNickname} title={author}>
            {showNickname ? props.nickname : cite(`~${author}`)}
          </Text>
          <Text fontSize='0' gray pr='3' display='inline-block'>{timeSent}</Text>
          <Text gray fontSize='0' display='inline-block'>
            {props.commentNumber} comments
          </Text>
        </Box>
      </Col>
    </Box>
  );
};

