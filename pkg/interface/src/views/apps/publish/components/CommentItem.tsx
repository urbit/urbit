import React, { useState } from "react";
import CommentInput from "./CommentInput";
import { Comment, NoteId } from "~/types/publish-update";
import { Contacts } from "~/types/contact-update";
import GlobalApi from "~/logic/api/global";
import { Box, Row } from "@tlon/indigo-react";
import styled from "styled-components";
import { Author } from "./Author";
import { GraphNode, TextContent } from "~/types/graph-update";
import tokenizeMessage from "~/logic/lib/tokenizeMessage";
import RichText from "~/views/components/RichText";
import { LocalUpdateRemoteContentPolicy } from "~/types";
import { MentionText } from "~/views/components/MentionText";

const ClickBox = styled(Box)`
  cursor: pointer;
  padding-left: ${(p) => p.theme.space[2]}px;
`;

interface CommentItemProps {
  pending?: boolean;
  comment: GraphNode;
  contacts: Contacts;
  book: string;
  ship: string;
  api: GlobalApi;
  hideNicknames: boolean;
  hideAvatars: boolean;
  remoteContentPolicy: LocalUpdateRemoteContentPolicy;
}

export function CommentItem(props: CommentItemProps) {
  const { ship, contacts, book, api, remoteContentPolicy } = props;
  const commentData = props.comment?.post;
  const comment = commentData.contents;

  const disabled = props.pending || window.ship !== commentData.author;

  const onDelete = async () => {
    await api.graph.removeNodes(ship, book, [commentData?.index]);
  };

  return (
    <Box mb={4} opacity={commentData?.pending ? "60%" : "100%"}>
      <Row bg="white" my={3}>
        <Author
          showImage
          contacts={contacts}
          ship={commentData?.author}
          date={commentData?.["time-sent"]}
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
          content={comment}
          remoteContentPolicy={remoteContentPolicy}
        />
      </Box>
    </Box>
  );
}

export default CommentItem;
