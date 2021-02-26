import React from 'react';
import { Link } from 'react-router-dom';
import styled from 'styled-components';

import { Box, Row, Text } from '@tlon/indigo-react';
import { Contacts } from '@urbit/api/contacts';
import { GraphNode } from '@urbit/api/graph';
import { Group } from '@urbit/api';

import GlobalApi from '~/logic/api/global';
import Author from '~/views/components/Author';
import { MentionText } from '~/views/components/MentionText';
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
}

export function CommentItem(props: CommentItemProps): ReactElement {
  const { ship, name, api, comment, group } = props;
  const [, post] = getLatestCommentRevision(comment);
  const disabled = props.pending || window.ship !== post?.author;

  const onDelete = async () => {
    await api.graph.removeNodes(ship, name, [comment.post?.index]);
  };

  const commentIndexArray = (comment.post?.index || '/').split('/');
  const commentIndex = commentIndexArray[commentIndexArray.length - 1];
  const updateUrl = `${props.baseUrl}/${commentIndex}`;

  return (
    <Box mb={4} opacity={post?.pending ? '60%' : '100%'}>
      <Row bg="white" my={3}>
        <Author
          showImage
          ship={post?.author}
          date={post?.['time-sent']}
          unread={props.unread}
          group={group}
          api={api}
        >
          {!disabled && (
            <Box display="inline-block" verticalAlign="middle">
              <Link to={updateUrl}>
                <Text
                  color="green"
                  ml={2}
                >
                  Update
                </Text>
              </Link>
              <ClickBox display="inline-block" color="red" onClick={onDelete}>
                <Text color='red'>Delete</Text>
              </ClickBox>
            </Box>
          )}
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
