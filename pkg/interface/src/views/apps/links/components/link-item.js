import React  from 'react';
import { Row, Col, Anchor, Box, Text } from '@tlon/indigo-react';

import { Sigil } from '~/logic/lib/sigil';
import { Link } from 'react-router-dom';
import { cite } from '~/logic/lib/util';

export const LinkItem = (props) => {
  const {
    node,
    nickname,
    color,
    avatar,
    resource,
    hideAvatars,
    hideNicknames
  } = props;

  const URLparser = new RegExp(
    /((?:([\w\d\.-]+)\:\/\/?){1}(?:(www)\.?){0,1}(((?:[\w\d-]+\.)*)([\w\d-]+\.[\w\d]+))){1}(?:\:(\d+)){0,1}((\/(?:(?:[^\/\s\?]+\/)*))(?:([^\?\/\s#]+?(?:.[^\?\s]+){0,1}){0,1}(?:\?([^\s#]+)){0,1})){0,1}(?:#([^#\s]+)){0,1}/
  );

  const author = node.post.author;
  const index = node.post.index.split('/').join('-');
  const size = node.children ? node.children.size : 0;
  const contents = node.post.contents;
  const hostname = URLparser.exec(contents[1].url) ? URLparser.exec(contents[1].url)[4] : null;

  const showAvatar = avatar && !hideAvatars;
  const showNickname = nickname && !hideNicknames;

  const img = showAvatar
    ? <img src={props.avatar} height={36} width={36} className="dib" />
    : <Sigil ship={`~${author}`} size={36} color={'#' + props.color} />;

  const baseUrl = props.baseUrl || `/~404/${resource}`;

  return (
    <Row minWidth='0' flexShrink='0' width="100%" alignItems="center" py={3} bg="white">
      {img}
      <Col minWidth='0' height="100%" width='100%' justifyContent="space-between" ml={2}>
        <Anchor
          lineHeight="tall"
          display='flex'
          style={{ textDecoration: 'none' }}
          href={contents[1].url}
          width="100%"
          target="_blank"
          rel="noopener noreferrer">
          <Text display='inline-block' overflow='hidden' style={{ textOverflow: 'ellipsis', whiteSpace: 'pre'}}> {contents[0].text}</Text>
            <Text ml="2" color="gray" display='inline-block' flexShrink='0'>{hostname} ↗</Text>
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
};

