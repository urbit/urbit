import React, { ReactElement } from 'react';
import { Link } from 'react-router-dom';
import styled from 'styled-components';

import { Box, Row, Text } from '@tlon/indigo-react';
import { Group, Contacts, GraphNode, removeNodes } from '@urbit/api';

import Author from '~/views/components/Author';
import { MentionText } from '~/views/components/MentionText';
import { getLatestCommentRevision } from '~/logic/lib/publish';
import useContactState from '~/logic/state/contacts';
import useApi from '~/logic/lib/useApi';

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
  group: Group;
}

export function CommentItem(props: CommentItemProps): ReactElement {
  const { ship, name, comment, group } = props;
  const contacts = useContactState(state => state.contacts);
  const [, post] = getLatestCommentRevision(comment);
  const disabled = props.pending || window.ship !== post?.author;
  const api = useApi();

  const onDelete = async () => {
    await api.poke(removeNodes(ship, name, [comment.post.index]));
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
