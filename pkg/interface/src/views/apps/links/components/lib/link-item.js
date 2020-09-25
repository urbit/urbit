import React, { Component } from 'react';
import { Row, Col, Anchor, Box, Text } from '@tlon/indigo-react';

import { Sigil } from '~/logic/lib/sigil';
import { Link } from 'react-router-dom';
import { cite } from '~/logic/lib/util';

export const LinkItem = (props) => {
  const {
    node,
    nickname,
    resource,
    hideAvatars,
    hideNicknames
  } = props;

  const author = node.post.author;
  const index = node.post.index.split('/').join('-');
  const size = node.children ? node.children.size : 0;
  const contents = node.post.contents;

  const showAvatar = props.avatar && !hideAvatars;
  const showNickname = nickname && !hideNicknames;

  const mono = showNickname ? 'inter white-d' : 'mono white-d';

  const img = showAvatar
    ? <img src={props.avatar} height={36} width={36} className="dib" />
    : <Sigil ship={`~${author}`} size={36} color={'#' + props.color} />;

  const baseUrl = props.baseUrl || `/~link/${resource}`;

  let hostname = '';
  try {
    const url = new URL(contents[1].url);
    hostname = url.hostname;
  } catch (e) {}


  return (
    <Row alignItems="center" py={3} bg="white">
      {img}
      <Col height="100%" justifyContent="space-between" ml={2}>
        <Anchor
          lineHeight="tall"
          textDecoration="none"
          href={contents[1].url}
          width="100%"
          target="_blank"
          rel="noopener noreferrer">
          <Text> {contents[0].text}</Text>
            <Text ml="2" color="gray">{hostname} ↗</Text> 
        </Anchor>
        <Box width="100%">
          <Text
            fontFamily={showNickname ? 'sans' : 'mono'} pr={2}>
            {showNickname ? nickname : cite(author) }
          </Text>
          <Link to={`${baseUrl}/${index}`}>
            <Text color="gray">{size} comments</Text>
          </Link>
        </Box>
      </Col>
    </Row>
  );
}

