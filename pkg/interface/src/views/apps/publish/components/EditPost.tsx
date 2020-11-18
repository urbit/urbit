import React from "react";
import _ from 'lodash';
import { PostFormSchema, PostForm } from "./NoteForm";
import { FormikHelpers } from "formik";
import GlobalApi from "~/logic/api/global";
import { RouteComponentProps, useLocation } from "react-router-dom";
import { GraphNode, TextContent, Association } from "~/types";
import { getLatestRevision, editPost } from "~/logic/lib/publish";
import {useWaitForProps} from "~/logic/lib/useWaitForProps";
interface EditPostProps {
  ship: string;
  noteId: number;
  note: GraphNode;
  api: GlobalApi;
  book: string;
}

export function EditPost(props: EditPostProps & RouteComponentProps) {
  const { note, book, noteId, api, ship, history } = props;
  const [revNum, title, body] = getLatestRevision(note);
  const location = useLocation();

  const waiter = useWaitForProps(props);
  const initial: PostFormSchema = {
    title,
    body,
  };

  const onSubmit = async (
    values: PostFormSchema,
    actions: FormikHelpers<PostFormSchema>
  ) => {
    const { title, body } = values;
    try {
      const newRev = revNum + 1;
      const nodes = editPost(newRev, noteId, title, body);
      await api.graph.addNodes(ship, book, nodes);
      await waiter(p => {
        const [rev] = getLatestRevision(p.note);
        return rev === newRev;
      });
      const noteUrl = _.dropRight(location.pathname.split('/'), 1).join('/');
      history.push(noteUrl);
    } catch (e) {
      console.error(e);
      actions.setStatus({ error: "Failed to edit notebook" });
    }
  };

  return (
    <PostForm
      initial={initial}
      onSubmit={onSubmit}
      submitLabel="Update"
      loadingText="Updating..."
    />
  );
}
