import React, { useState } from "react";
import CommentInput from "./CommentInput";
import { Comment, NoteId } from "~/types/publish-update";
import { Contacts } from "~/types/contact-update";
import GlobalApi from "~/logic/api/global";
import { Box, Row } from "@tlon/indigo-react";
import styled from "styled-components";
import { Author } from "./Author";
import tokenizeMessage from '~/logic/lib/tokenizeMessage';
import RichText from '~/views/components/RichText';

const ClickBox = styled(Box)`
  cursor: pointer;
  padding-left: ${p => p.theme.space[2]}px;
`;

interface CommentItemProps {
  pending?: boolean;
  comment: Comment;
  contacts: Contacts;
  book: string;
  ship: string;
  api: GlobalApi;
  note: NoteId;
  hideNicknames: boolean;
  hideAvatars: boolean;
}

export function CommentItem(props: CommentItemProps) {
  const { ship, contacts, book, note, api, remoteContentPolicy } = props;
  const [editing, setEditing] = useState<boolean>(false);
  const commentPath = Object.keys(props.comment)[0];
  const commentData = props.comment[commentPath];
  const content = tokenizeMessage(commentData.content).flat().join(' ');

  const disabled = props.pending || window.ship !== commentData.author.slice(1);

  const onUpdate = async ({ comment }) => {
    await api.publish.updateComment(
      ship.slice(1),
      book,
      note,
      commentPath,
      comment
    );
    setEditing(false);
  };

  const onDelete = async () => {
    await api.publish.deleteComment(ship.slice(1), book, note, commentPath);
  };

  return (
    <Box mb={4} opacity={props.pending ? "60%" : "100%"}>
      <Row bg="white" my={3}>
        <Author
          showImage
          contacts={contacts}
          ship={commentData?.author}
          date={commentData["date-created"]}
          hideAvatars={props.hideAvatars}
          hideNicknames={props.hideNicknames}
        >
          {!disabled && !editing && (
            <>
              <ClickBox color="green" onClick={() => setEditing(true)}>
                Edit
              </ClickBox>
              <ClickBox color="red" onClick={onDelete}>
                Delete
              </ClickBox>
            </>
          )}
          {editing && (
            <ClickBox onClick={() => setEditing(false)} color="red">
              Cancel
            </ClickBox>
          )}
        </Author>
      </Row>
      <Box mb={2}>
        {editing
          ? <CommentInput
            onSubmit={onUpdate}
            initial={commentData.content}
            label="Update"
          />
        : <RichText className="f9 white-d" remoteContentPolicy={remoteContentPolicy}>{content}</RichText>}
      </Box>
    </Box>
  );
}

export default CommentItem;
