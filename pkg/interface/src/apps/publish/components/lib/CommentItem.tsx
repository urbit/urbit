import React, { Component, useState, useCallback } from "react";
import moment from "moment";
import { Sigil } from "../../../../lib/sigil";
import CommentInput from "./CommentInput";
import { uxToHex, cite } from "../../../../lib/util";
import { Comment, NoteId } from "../../../../types/publish-update";
import { Contacts } from "../../../../types/contact-update";
import GlobalApi from "../../../../api/global";
import { Button } from "@tlon/indigo-react";

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
  const [editing, setEditing] = useState<boolean>(false);
  const pending = props.pending ? "o-60" : "";
  const commentPath = Object.keys(props.comment)[0];
  const commentData = props.comment[commentPath];
  const content = commentData.content.split("\n").map((line, i) => {
    return (
      <p className="mb2" key={i}>
        {line}
      </p>
    );
  });
  const date = moment(commentData["date-created"]).fromNow();

  const contact =
    commentData.author.substr(1) in props.contacts
      ? props.contacts[commentData.author.substr(1)]
      : null;

  let name = commentData.author;
  let color = "#000000";
  let classes = "mix-blend-diff";
  let avatar: string | null = null;
  if (contact) {
    name = contact.nickname.length > 0 ? contact.nickname : commentData.author;
    color = `#${uxToHex(contact.color)}`;
    classes = "";
    avatar = contact.avatar;
  }

  const img =
    avatar !== null ? (
      <img src={avatar} height={24} width={24} className="dib" />
    ) : (
      <Sigil
        ship={commentData.author}
        size={24}
        color={color}
        classes={classes}
      />
    );

  if (name === commentData.author) {
    name = cite(commentData.author);
  }

  const disabled = props.pending || window.ship !== commentData.author.slice(1);

  const onUpdate = useCallback(
    async ({ comment }) => {
      const action = {
        "edit-comment": {
          who: props.ship.slice(1),
          book: props.book,
          note: props.note,
          body: comment,
          comment: commentPath,
        },
      };
      await props.api.publish.publishAction(action);

      setEditing(false);
    },
    [props.api, props.ship, props.note, props.book, commentPath, setEditing]
  );

  return (
    <div className={"mb8 " + pending}>
      <div className="flex mv3 bg-white bg-gray0-d">
        {img}
        <div
          className={"f9 mh2 pt1 " + (contact?.nickname ? null : "mono")}
          title={commentData.author}
        >
          {name}
        </div>
        <div className="f9 gray3 pt1">{date}</div>
        {!disabled && !editing && (
          <>
            <div
              onClick={() => setEditing(true)}
              className="green2 pointer ml2 f9 pt1"
            >
              Edit
            </div>
            <div className="red2 pointer ml2 f9 pt1">Delete</div>
          </>
        )}
        {editing && (
          <div
            onClick={() => setEditing(false)}
            className="red2 pointer ml2 f9 pt1"
          >
            Cancel
          </div>
        )}
      </div>
      <div className="f8 lh-solid mb2">
        {!editing && content}
        {editing && (
          <CommentInput
            onSubmit={onUpdate}
            initial={commentData.content}
            label="Update"
          />
        )}
      </div>
    </div>
  );
}

export default CommentItem;
