import { Association } from '@urbit/api';
import { Graph } from '@urbit/api/graph';
import { FormikHelpers } from 'formik';
import React from 'react';
import { RouteComponentProps } from 'react-router-dom';
import GlobalApi from '~/logic/api/global';
import { newPost } from '~/logic/lib/publish';
import { useWaitForProps } from '~/logic/lib/useWaitForProps';
import { PostForm, PostFormSchema } from './NoteForm';

interface NewPostProps {
  api: GlobalApi;
  book: string;
  ship: string;
  graph: Graph;
  association: Association;
  baseUrl: string;
}

export default function NewPost(props: NewPostProps & RouteComponentProps) {
  const { api, book, ship, history } = props;

  const waiter = useWaitForProps(props, 20000);

  const onSubmit = async (
    values: PostFormSchema,
    actions: FormikHelpers<PostFormSchema>
  ) => {
    const { title, body } = values;
    try {
      const [noteId, nodes] = newPost(title, body);
      await api.graph.addNodes(ship, book, nodes);
      history.push(`${props.baseUrl}`);
    } catch (e) {
      console.error(e);
      actions.setStatus({ error: 'Posting note failed' });
    }
  };

  const initialValues: PostFormSchema = {
    title: '',
    body: ''
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
