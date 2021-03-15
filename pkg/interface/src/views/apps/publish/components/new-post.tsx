import React from 'react';
import { FormikHelpers } from 'formik';
import GlobalApi from '~/logic/api-old/global';
import { useWaitForProps } from '~/logic/lib/useWaitForProps';
import { RouteComponentProps } from 'react-router-dom';
import { PostForm, PostFormSchema } from './NoteForm';
import { createPost } from '~/logic/api-old/graph';
import { Graph } from '@urbit/api/graph';
import { addNodes, Association, graph } from '@urbit/api';
import { StorageState } from '~/types';
import { newPost } from '~/logic/lib/publish';
import useApi from '~/logic/api';

interface NewPostProps {
  book: string;
  ship: string;
  graph: Graph;
  association: Association;
  baseUrl: string;
}

export default function NewPost(props: NewPostProps & RouteComponentProps) {
  const { book, ship, history } = props;
  const api = useApi();

  const waiter = useWaitForProps(props, 20000);

  const onSubmit = async (
    values: PostFormSchema,
    actions: FormikHelpers<PostFormSchema>
  ) => {
    const { title, body } = values;
    try {
      const [noteId, nodes] = newPost(title, body);
      await api.poke(graph.addNodes(ship, book, nodes));
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
