import React, { useState, useEffect, useCallback } from "react";
import { Col } from "@tlon/indigo-react";
import { CommentItem } from "./CommentItem";
import CommentInput from "./CommentInput";
import { dateToDa } from "~/logic/lib/util";
import { Comment, Note, NoteId } from "~/types/publish-update";
import { Contacts } from "~/types/contact-update";
import _ from "lodash";
import GlobalApi from "~/logic/api/global";
import { FormikHelpers } from "formik";
import {GraphNode, Graph} from "~/types/graph-update";
import {createPost} from "~/logic/api/graph";
import { LocalUpdateRemoteContentPolicy } from "~/types";
import {scanForMentions} from "~/logic/lib/graph";

interface CommentsProps {
  comments: GraphNode;
  book: string;
  note: GraphNode;
  ship: string;
  contacts: Contacts;
  api: GlobalApi;
  hideAvatars: boolean;
  hideNicknames: boolean;
  remoteContentPolicy: LocalUpdateRemoteContentPolicy;
}

export function Comments(props: CommentsProps) {
  const { comments, ship, book, note, api } = props;

  const onSubmit = async (
    { comment },
    actions: FormikHelpers<{ comment: string }>
  ) => {
    try {
      const content = scanForMentions(comment)
      const post = createPost(content, comments?.post?.index);
      await api.graph.addPost(ship, book, post)
      actions.resetForm();
      actions.setStatus({ success: null });
    } catch (e) {
      console.error(e);
      actions.setStatus({ error: e.message });
    }
  };

  return (
    <Col>
      <CommentInput onSubmit={onSubmit} />
      {Array.from(comments.children).reverse().map(([idx, comment]) => (
        <CommentItem
          comment={comment}
          key={idx.toString()}
          contacts={props.contacts}
          api={api}
          book={book}
          ship={ship}
          hideNicknames={props.hideNicknames}
          hideAvatars={props.hideAvatars}
          remoteContentPolicy={props.remoteContentPolicy}
        />
      ))}
    </Col>
  );
}

export default Comments;
