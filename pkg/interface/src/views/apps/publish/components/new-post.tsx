import React from 'react';
import { FormikHelpers } from 'formik';
import { RouteComponentProps } from 'react-router-dom';

import { Association, Graph } from '@urbit/api';

import { useWaitForProps } from '~/logic/lib/useWaitForProps';
import { PostForm, PostFormSchema } from './NoteForm';
import { newPost } from '~/logic/lib/publish';
import { S3State } from '~/types';
import useApi from '~/logic/lib/useApi';
import { addNodes } from '@urbit/api/graph';

interface NewPostProps {
  book: string;
  ship: string;
  graph: Graph;
  association: Association;
  baseUrl: string;
  s3: S3State;
}

export default function NewPost(props: NewPostProps & RouteComponentProps) {
  const { book, ship, history } = props;

  const waiter = useWaitForProps(props, 20000);
  const api = useApi();

  const onSubmit = async (
    values: PostFormSchema,
    actions: FormikHelpers<PostFormSchema>
  ) => {
    const { title, body } = values;
    try {
      const [noteId, nodes] = newPost(title, body);
      await api.poke(addNodes(ship, book, nodes));
      await waiter(p =>
        p.graph.has(noteId) && !p.graph.get(noteId)?.post?.pending
      );
      history.push(`${props.baseUrl}/note/${noteId}`);
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
      s3={props.s3}
    />
  );
}
