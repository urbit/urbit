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
import {GraphNode, TextContent} from "~/types/graph-update";

const ClickBox = styled(Box)`
  cursor: pointer;
  padding-left: ${p => p.theme.space[2]}px;
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
}

export function CommentItem(props: CommentItemProps) {
  const { ship, contacts, book, api } = props;
  const [editing, setEditing] = useState<boolean>(false);
  const commentData = props.comment?.post;
  const comment = commentData.contents[0] as TextContent;
  const content = comment.text.split("\n").map((line, i) => {
    return (
      <Text mb="2" key={i}>
        {line}
      </Text>
    );
  });

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
          date={commentData["date-created"]}
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
        {!editing && content}
      </Box>
    </Box>
  );
}

export default CommentItem;
