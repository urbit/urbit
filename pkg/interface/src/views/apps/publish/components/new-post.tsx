import { addNodes, Association } from '@urbit/api';
import { Graph } from '@urbit/api/graph';
import { FormikHelpers } from 'formik';
import React from 'react';
import { RouteComponentProps } from 'react-router-dom';
import { newPost } from '~/logic/lib/publish';
import { PostForm, PostFormSchema } from './NoteForm';
import airlock from '~/logic/api';

interface NewPostProps {
  book: string;
  ship: string;
  graph: Graph;
  association: Association;
  baseUrl: string;
}

export default function NewPost(props: NewPostProps & RouteComponentProps) {
  const { book, ship, history } = props;

  const onSubmit = async (
    values: PostFormSchema,
    actions: FormikHelpers<PostFormSchema>
  ) => {
    const { title, body } = values;
    try {
      const [, nodes] = newPost(title, body);
      await airlock.thread(addNodes(ship, book, nodes));
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
