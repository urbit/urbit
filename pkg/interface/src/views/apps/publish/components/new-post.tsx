import React from 'react';
import { FormikHelpers } from 'formik';
import GlobalApi from '~/logic/api/global';
import { useWaitForProps } from '~/logic/lib/useWaitForProps';
import { RouteComponentProps } from 'react-router-dom';
import { PostForm, PostFormSchema } from './NoteForm';
import { createPost } from '~/logic/api/graph';
import { Graph } from '@urbit/api/graph';
import { Association } from '@urbit/api';
import { StorageState } from '~/types';
import { newPost } from '~/logic/lib/publish';

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
