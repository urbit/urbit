import React from "react";
import { stringToSymbol } from "~/logic/lib/util";
import { FormikHelpers } from "formik";
import GlobalApi from "~/logic/api/global";
import { useWaitForProps } from "~/logic/lib/useWaitForProps";
import { Notebook } from "~/types/publish-update";
import { RouteComponentProps } from "react-router-dom";
import { PostForm, PostFormSchema } from "./NoteForm";

interface NewPostProps {
  api: GlobalApi;
  book: string;
  ship: string;
  notebook: Notebook;
  baseUrl: string;
}

export default function NewPost(props: NewPostProps & RouteComponentProps) {
  const { api, book, notebook, ship, history } = props;

  const waiter = useWaitForProps(props, 20000);

  const onSubmit = async (
    values: PostFormSchema,
    actions: FormikHelpers<PostFormSchema>
  ) => {
    let noteId = stringToSymbol(values.title);
    const { title, body } = values;
    const host = ship.slice(1);

    try {
      try {
        await api.publish.newNote(host, book, noteId, title, body);
      } catch (e) {
        if (e.includes("note already exists")) {
          const timestamp = Math.floor(Date.now() / 1000);
          noteId = `${noteId}-${timestamp}`;
          await api.publish.newNote(host, book, noteId, title, body);
        } else {
          throw e;
        }
      }
      await waiter((p) => {
        return !!p?.notebook?.notes[noteId];
      });
      history.push(`${props.baseUrl}/note/${noteId}`);
    } catch (e) {
      console.error(e);
      actions.setStatus({ error: "Posting note failed" });
    }
  };

  const initialValues: PostFormSchema = {
    title: "",
    body: "",
  };

  return (
    <PostForm
      initial={initialValues}
      onSubmit={onSubmit}
      submitLabel="Publish"
      loadingText="Posting..."
    />
  );
}
