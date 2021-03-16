import React, {useEffect, useRef} from 'react';
import { Link } from 'react-router-dom';
import styled from 'styled-components';

import { Box, Row, Text } from '@tlon/indigo-react';
import { Contacts } from '@urbit/api/contacts';
import { GraphNode } from '@urbit/api/graph';
import { Group } from '@urbit/api';

import GlobalApi from '~/logic/api/global';
import Author from '~/views/components/Author';
import { MentionText } from '~/views/components/MentionText';
import { roleForShip } from '~/logic/lib/group';
import { getLatestCommentRevision } from '~/logic/lib/publish';

const ClickBox = styled(Box)`
  cursor: pointer;
  padding-left: ${p => p.theme.space[2]}px;
`;

interface CommentItemProps {
  pending?: boolean;
  comment: GraphNode;
  baseUrl: string;
  unread: boolean;
  name: string;
  ship: string;
  api: GlobalApi;
  group: Group;
  highlighted: boolean;
}

export function CommentItem(props: CommentItemProps): ReactElement {
  const { ship, name, api, comment, group } = props;
  const ref = useRef<HTMLElement | null>(null);
  const [, post] = getLatestCommentRevision(comment);
  const disabled = props.pending;

  const onDelete = async () => {
    await api.graph.removeNodes(ship, name, [comment.post?.index]);
  };

  const commentIndexArray = (comment.post?.index || '/').split('/');
  const commentIndex = commentIndexArray[commentIndexArray.length - 1];

  const adminLinks: JSX.Element[] = [];
  const ourRole = roleForShip(group, window.ship);
  if (window.ship == post?.author && !disabled) {
    adminLinks.push(
      <Link to={{ pathname: props.baseUrl, search: `?edit=${commentIndex}`}}>
        <Text
          color="blue"
          ml={2}
        >
          Update
        </Text>
      </Link>
    )
  };

  if ((window.ship == post?.author || ourRole == "admin") && !disabled) {
    adminLinks.push(
      <ClickBox display="inline-block" color="red" onClick={onDelete}>
        <Text color='red'>Delete</Text>
      </ClickBox>
    )
  };

  useEffect(() => {
    if(props.highlighted) {
      ref.current.scrollIntoView();
    }

  }, [props.highlighted]);

  return (
    <Box ref={ref} border={props.highlighted ? 1 : 0} borderRadius={1} borderColor="blue" mb={4} opacity={post?.pending ? '60%' : '100%'}>
      <Row my={3}>
        <Author
          showImage
          ship={post?.author}
          date={post?.['time-sent']}
          unread={props.unread}
          group={group}
        >
          <Row alignItems="center">
            {adminLinks}
          </Row>
        </Author>
      </Row>
      <Box mb={2}>
        <MentionText
          group={group}
          content={post?.contents}
        />
      </Box>
    </Box>
  );
}

export default CommentItem;
