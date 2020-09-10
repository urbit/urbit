import React from "react";
import { PostFormSchema, PostForm } from "./NoteForm";
import { FormikHelpers } from "formik";
import GlobalApi from "~/logic/api/global";
import { RouteComponentProps } from "react-router-dom";
import { GraphNode, TextContent } from "~/types";
interface EditPostProps {
  ship: string;
  noteId: number;
  note: GraphNode;
  api: GlobalApi;
  book: string;
}

export function EditPost(props: EditPostProps & RouteComponentProps) {
  const { note, book, noteId, api, ship, history } = props;
  const [title, file] = note.post.contents as TextContent[];
  const body = file.text.slice(file.text.indexOf(";>") + 2);
  const initial: PostFormSchema = {
    title: title.text,
    body,
  };

  const onSubmit = async (
    values: PostFormSchema,
    actions: FormikHelpers<PostFormSchema>
  ) => {
    const { title, body } = values;
    try {
      // graph-store does not support editing nodes
      throw new Error("Unsupported");
    } catch (e) {
      console.error(e);
      actions.setStatus({ error: "Failed to edit notebook" });
    }
  };

  return (
    <PostForm
      initial={initial}
      onSubmit={onSubmit}
      submitLabel={`Update ${title.text}`}
      loadingText="Updating..."
    />
  );
}
