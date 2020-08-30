import React, { Component } from "react";
import { PostFormSchema, PostForm } from "./NoteForm";
import { Note } from "../../../../types/publish-update";
import { FormikHelpers } from "formik";
import GlobalApi from "../../../../api/global";
import { RouteComponentProps } from "react-router-dom";
interface EditPostProps {
  ship: string;
  noteId: string;
  note: Note;
  api: GlobalApi;
  book: string;
}

export function EditPost(props: EditPostProps & RouteComponentProps) {
  const { note, book, noteId, api, ship, history } = props;
  const body = note.file.slice(note.file.indexOf(";>") + 2);
  const initial: PostFormSchema = {
    title: note.title,
    body,
  };

  const onSubmit = async (
    values: PostFormSchema,
    actions: FormikHelpers<PostFormSchema>
  ) => {
    const { title, body } = values;
    const host = ship.slice(1);
    try {
      await api.publish.editNote(host, book, noteId, title, body);
      history.push(`/~publish/notebook/${ship}/${book}/note/${noteId}`);
    } catch (e) {
      actions.setStatus({ error: "Failed to edit notebook" });
    }
  };

  return (
    <PostForm
      initial={initial}
      onSubmit={onSubmit}
      submitLabel={`Update ${note.title}`}
      loadingText="Updating..."
    />
  );
}
