import React from 'react';
import { Contacts } from '~/types/contact-update';
import GlobalApi from '~/logic/api/global';
import { Box, Row } from '@tlon/indigo-react';
import styled from 'styled-components';
import { Author } from '~/views/apps/publish/components/Author';
import { GraphNode, TextContent } from '~/types/graph-update';
import tokenizeMessage from '~/logic/lib/tokenizeMessage';
import { LocalUpdateRemoteContentPolicy } from '~/types';
import { MentionText } from '~/views/components/MentionText';
import { getLatestCommentRevision } from '~/logic/lib/publish';

const ClickBox = styled(Box)`
  cursor: pointer;
  padding-left: ${p => p.theme.space[2]}px;
`;

interface CommentItemProps {
  pending?: boolean;
  comment: GraphNode;
  contacts: Contacts;
  name: string;
  ship: string;
  api: GlobalApi;
  hideNicknames: boolean;
  hideAvatars: boolean;
  remoteContentPolicy: LocalUpdateRemoteContentPolicy;
}

export function CommentItem(props: CommentItemProps) {
  const { ship, contacts, name, api, remoteContentPolicy, comment } = props;
  const [revNum, post] = getLatestCommentRevision(comment);
  const disabled = props.pending || window.ship !== post?.author;

  const onDelete = async () => {
    await api.graph.removeNodes(ship, name, [comment.post?.index]);
  };

  return (
    <Box mb={4} opacity={post?.pending ? '60%' : '100%'}>
      <Row bg="white" my={3}>
        <Author
          showImage
          contacts={contacts}
          ship={post?.author}
          date={post?.['time-sent']}
          hideAvatars={props.hideAvatars}
          hideNicknames={props.hideNicknames}
        >
          {!disabled && (
            <>
              <ClickBox color="red" onClick={onDelete}>
                Delete
              </ClickBox>
            </>
          )}
        </Author>
      </Row>
      <Box mb={2}>
        <MentionText
          contacts={contacts}
          content={post?.contents}
          remoteContentPolicy={remoteContentPolicy}
        />
      </Box>
    </Box>
  );
}

export default CommentItem;
