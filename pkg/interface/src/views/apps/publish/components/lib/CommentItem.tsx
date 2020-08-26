import React, { useState } from "react";
import moment from "moment";
import { Sigil } from "~/logic/lib/sigil";
import CommentInput from "./CommentInput";
import { uxToHex, cite } from "~/logic/lib/util";
import { Comment, NoteId } from "~/types/publish-update";
import { Contacts } from "~/types/contact-update";
import GlobalApi from "~/logic/api/global";
import { Button, Box, Row, Text } from "@tlon/indigo-react";
import styled from "styled-components";
import { Author } from "./Author";

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
}

export function CommentItem(props: CommentItemProps) {
  const { ship, contacts, book, note, api } = props;
  const [editing, setEditing] = useState<boolean>(false);
  const commentPath = Object.keys(props.comment)[0];
  const commentData = props.comment[commentPath];
  const content = commentData.content.split("\n").map((line, i) => {
    return (
      <Text className="mb2" key={i}>
        {line}
      </Text>
    );
  });

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
        {!editing && content}
        {editing && (
          <CommentInput
            onSubmit={onUpdate}
            initial={commentData.content}
            label="Update"
          />
        )}
      </Box>
    </Box>
  );
}

export default CommentItem;
