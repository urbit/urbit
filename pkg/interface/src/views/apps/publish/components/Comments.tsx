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
import { LocalUpdateRemoteContentPolicy } from "~/types";

interface CommentsProps {
  comments: Comment[];
  book: string;
  noteId: NoteId;
  note: Note;
  ship: string;
  contacts: Contacts;
  api: GlobalApi;
  numComments: number;
  enabled: boolean;
  hideAvatars: boolean;
  hideNicknames: boolean;
  remoteContentPolicy: LocalUpdateRemoteContentPolicy;
}

export function Comments(props: CommentsProps) {
  const { comments, ship, book, note, api, noteId, numComments } = props;
  const [pending, setPending] = useState<string[]>([]);

  useEffect(() => {
    _.forEach(comments, (comment: Comment) => {
      const { content } = comment[Object.keys(comment)[0]];
      setPending((p) => p.filter((p) => p !== content));
    });
  }, [numComments]);

  const onSubmit = async (
    { comment },
    actions: FormikHelpers<{ comment: string }>
  ) => {
    setPending((p) => [...p, comment]);
    const action = {
      "new-comment": {
        who: ship.slice(1),
        book: book,
        note: noteId,
        body: comment,
      },
    };
    try {
      await api.publish.publishAction(action);
      actions.resetForm();
      actions.setStatus({ success: null });
    } catch (e) {
      actions.setStatus({ error: e.message });
    }
  };

  return (
    <Col>
      <CommentInput onSubmit={onSubmit} />
      {Array.from(pending).map((com, i) => {
        const da = dateToDa(new Date());
        const ship = `~${window.ship}`;
        const comment = {
          [da]: {
            author: ship,
            content: com,
            "date-created": Math.round(new Date().getTime()),
          },
        } as Comment;
        return (
          <CommentItem
            comment={comment}
            key={i}
            contacts={props.contacts}
            ship={ship}
            pending={true}
            hideNicknames={props.hideNicknames}
            hideAvatars={props.hideAvatars}
            remoteContentPolicy={props.remoteContentPolicy}
          />
        );
      })}
      {props.comments.map((com, i) => (
        <CommentItem
          comment={com}
          key={i}
          contacts={props.contacts}
          api={api}
          book={book}
          ship={ship}
          note={note["note-id"]}
          hideNicknames={props.hideNicknames}
          hideAvatars={props.hideAvatars}
          remoteContentPolicy={props.remoteContentPolicy}
        />
      ))}
    </Col>
  );
}

export default Comments;
