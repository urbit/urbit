import React from 'react';
import { FormikHelpers } from 'formik';
import { useWaitForProps } from '~/logic/lib/useWaitForProps';
import { RouteComponentProps } from 'react-router-dom';
import { PostForm, PostFormSchema } from './NoteForm';
import { Graph } from '@urbit/api/graph';
import { Association, graph } from '@urbit/api';
import { StorageState } from '~/types';
import { newPost } from '~/logic/lib/publish';
import useApi from '~/logic/api';
import useGraphState from '~/logic/state/graph';

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
  const addNodes = useGraphState(state => state.addNodes);

  const waiter = useWaitForProps(props, 20000);

  const onSubmit = async (
    values: PostFormSchema,
    actions: FormikHelpers<PostFormSchema>
  ) => {
    const { title, body } = values;
    try {
      const [noteId, nodes] = newPost(title, body);
      await addNodes(ship, book, nodes);
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
