import React, { useState, useEffect, useCallback } from "react";
import { CommentItem } from "./CommentItem";
import CommentInput from "./CommentInput";
import { dateToDa } from "../../../../lib/util";
import { Spinner } from "../../../../components/Spinner";
import { Comment, Note, NoteId } from "../../../../types/publish-update";
import { Contacts } from "../../../../types/contact-update";
import _ from "lodash";
import GlobalApi from "../../../../api/global";
import { FormikHelpers } from "formik";

/**
 *
  commentUpdate(idx, body) {
    const path = Object.keys(this.props.comments[idx])[0];
    const comment = {
      'edit-comment': {
        who: this.props.ship.slice(1),
        book: this.props.book,
        note: this.props.note,
        body: body,
        comment: path
      }
    };

    this.setState({ awaiting: 'edit' });

    this.props.api.publish
      .publishAction(comment)
      .then(() => {
    this.setState({ awaiting: null, editing: null });
    });
  }

  commentDelete(idx) {
    const path = Object.keys(this.props.comments[idx])[0];
    const comment = {
      'del-comment': {
        who: this.props.ship.slice(1),
        book: this.props.book,
        note: this.props.note,
        comment: path
      }
    };

    this.setState({ awaiting: { kind: 'del', what: idx } });
    this.props.api.publish
      .publishAction(comment)
      .then(() => {
 this.setState({ awaiting: null });
});
  }


 */

interface CommentsProps {
  comments: Comment[];
  book: string;
  noteId: NoteId;
  note: Note;
  ship: string;
  contacts: Contacts;
  api: GlobalApi;
  enabled: boolean;
}

export function Comments(props: CommentsProps) {
  const { comments, ship, book, note, api, noteId } = props;
  const [pending, setPending] = useState<string[]>([]);
  const [awaiting, setAwaiting] = useState<string | null>(null);

  useEffect(() => {
    _.forEach(comments, (comment: Comment) => {
      const { content } = comment[Object.keys(comment)[0]];
      setPending((p) => p.filter((p) => p === content));
    });
  }, [comments.length]);

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

  if (!props.enabled) {
    return null;
  }

  const pendingArray = Array.from(pending).map((com, i) => {
    const da = dateToDa(new Date());
    const comment = {
      [da]: {
        author: `~${window.ship}`,
        content: com,
        "date-created": Math.round(new Date().getTime()),
      },
    };
    return (
      <CommentItem
        comment={comment}
        key={i}
        contacts={props.contacts}
        pending={true}
      />
    );
  });

  const commentArray = props.comments.map((com, i) => {
    return (
      <CommentItem
        comment={com}
        key={i}
        contacts={props.contacts}
        api={api}
        book={book}
        ship={ship}
        note={note}
      />
    );
  });

  const spinnerText =
    awaiting === "new"
      ? "Posting commment..."
      : awaiting === "edit"
      ? "Updating comment..."
      : "Deleting comment...";

  return (
    <div>
      <div className="mv8 relative">
        <CommentInput onSubmit={onSubmit} />
      </div>
      {pendingArray}
      {commentArray}
    </div>
  );
}

export default Comments;
