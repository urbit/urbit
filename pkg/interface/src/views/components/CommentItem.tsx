import React, { useCallback } from 'react';
import { Link } from "react-router-dom";
import { Contacts } from '~/types/contact-update';
import GlobalApi from '~/logic/api/global';
import { Box, Row, Text } from '@tlon/indigo-react';
import styled from 'styled-components';
import Author from '~/views/components/Author';
import { Post } from '~/types/graph-update';
import { Group } from '~/types';
import { MentionText } from '~/views/components/MentionText';

const ClickBox = styled(Box)`
  cursor: pointer;
  padding-left: ${p => p.theme.space[2]}px;
`;

interface CommentItemProps {
  pending?: boolean;
  post: Post;
  baseUrl: string;
  contacts: Contacts;
  unread: boolean;
  name: string;
  ship: string;
  api: GlobalApi;
  group: Group;
  index: string;
}

export function CommentItem(props: CommentItemProps) {
  const { ship, contacts, name, api, post, index, group } = props;
  const disabled = props.pending || window.ship !== post?.author;

  const onDelete = useCallback(async () => {
    await api.graph.removeNodes(ship, name, [index]);
  }, [index])

  const commentIndexArray = (index || '/').split('/');
  const commentIndex = commentIndexArray[commentIndexArray.length - 1];
  const updateUrl = `${props.baseUrl}/${commentIndex}`

  return (
    <Box mb={4} opacity={post?.pending ? '60%' : '100%'}>
      <Row bg="white" my={3}>
        <Author
          showImage
          contacts={contacts}
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
              <ClickBox display="inline-block" onClick={onDelete}>
                <Text color="red">Delete</Text>
              </ClickBox>
            </Box>
          )}
        </Author>
      </Row>
      <Box mb={2}>
        <MentionText
          contacts={contacts}
          group={group}
          content={post?.contents}
        />
      </Box>
    </Box>
  );
}

export default CommentItem;
