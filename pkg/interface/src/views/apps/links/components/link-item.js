import React from 'react';
import { Row, Col, Anchor, Box, Text, BaseImage } from '@tlon/indigo-react';

import { Sigil } from '~/logic/lib/sigil';
import { Link } from 'react-router-dom';
import { cite } from '~/logic/lib/util';
import { Author } from "~/views/apps/publish/components/Author";

import { roleForShip } from '~/logic/lib/group';

export const LinkItem = (props) => {
  const {
    node,
    nickname,
    avatar,
    contacts,
    unread,
    resource,
    hideAvatars,
    hideNicknames,
    api,
    group
  } = props;

  const URLparser = new RegExp(
    /((?:([\w\d\.-]+)\:\/\/?){1}(?:(www)\.?){0,1}(((?:[\w\d-]+\.)*)([\w\d-]+\.[\w\d]+))){1}(?:\:(\d+)){0,1}((\/(?:(?:[^\/\s\?]+\/)*))(?:([^\?\/\s#]+?(?:.[^\?\s]+){0,1}){0,1}(?:\?([^\s#]+)){0,1})){0,1}(?:#([^#\s]+)){0,1}/
  );

  const author = node.post.author;
  const index = node.post.index.split('/')[1];
  const size = node.children ? node.children.size : 0;
  const date = node.post['time-sent'];
  const contents = node.post.contents;
  const hostname = URLparser.exec(contents[1].url) ? URLparser.exec(contents[1].url)[4] : null;

  const showAvatar = avatar && !hideAvatars;
  const showNickname = nickname && !hideNicknames;

  const img = showAvatar
    ? <BaseImage display='inline-block' src={props.avatar} height={36} width={36} />
    : <Sigil ship={`~${author}`} size={36} color={'#' + props.color} />;

  const baseUrl = props.baseUrl || `/~404/${resource}`;

  const ourRole = group ? roleForShip(group, window.ship) : undefined;
  const [ship, name] = resource.split('/');

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
          rel="noopener noreferrer"
        >
          <Text display='inline-block' overflow='hidden' style={{ textOverflow: 'ellipsis', whiteSpace: 'pre' }}>{contents[0].text}</Text>
            <Text ml="2" color="gray" display='inline-block' flexShrink='0'>{hostname} ↗</Text>
        </Anchor>
        <Row alignItems="center" width="100%">
          <Author
            contacts={contacts}
            ship={author}
            hideAvatars={hideAvatars}
            hideNicknames={hideNicknames}
            unread={unread}
            date={date}
          >
          <Link to={`${baseUrl}/${index}`}>
            <Text ml="2" color="gray">{size} comments</Text>
          </Link>
          {(ourRole === 'admin' || node.post.author === window.ship)
            && (<Text color='red' ml='2' cursor='pointer' onClick={() => api.graph.removeNodes(`~${ship}`, name, [node.post.index])}>Delete</Text>)}
          </Author>
        </Row>
      </Col>
    </Row>
  );
};

