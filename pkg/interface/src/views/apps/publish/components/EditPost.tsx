import React, { ReactElement } from 'react';
import _ from 'lodash';
import { FormikHelpers } from 'formik';
import { RouteComponentProps, useHistory, useLocation } from 'react-router-dom';

import { GraphNode, addNodes } from '@urbit/api';

import { PostFormSchema, PostForm } from './NoteForm';
import GlobalApi from '~/logic/api/global';
import { getLatestRevision, editPost } from '~/logic/lib/publish';
import { useWaitForProps } from '~/logic/lib/useWaitForProps';
import { S3State } from '~/types';
import useApi from '~/logic/lib/useApi';

interface EditPostProps {
  ship: string;
  noteId: number;
  note: GraphNode;
  api: GlobalApi;
  book: string;
  s3: S3State;
}

export function EditPost(props: EditPostProps & RouteComponentProps): ReactElement {
  const { note, book, noteId, ship, s3 } = props;
  const history = useHistory();
  const [revNum, title, body] = getLatestRevision(note);
  const location = useLocation();

  const waiter = useWaitForProps(props);
  const initial: PostFormSchema = {
    title,
    body
  };

  const api = useApi();

  const onSubmit = async (
    values: PostFormSchema,
    actions: FormikHelpers<PostFormSchema>
  ): Promise<void> => {
    const { title, body } = values;
    try {
      const newRev = revNum + 1;
      const nodes = editPost(newRev, noteId, title, body);
      await api.poke(addNodes(ship, book, nodes));
      await waiter((p) => {
        const [rev] = getLatestRevision(p.note);
        return rev === newRev;
      });
      const noteUrl = _.dropRight(location.pathname.split('/'), 1).join('/');
      history.push(noteUrl);
    } catch (e) {
      console.error(e);
      actions.setStatus({ error: 'Failed to edit notebook' });
    }
  };

  return (
    <PostForm
      initial={initial}
      cancel
      onSubmit={onSubmit}
      s3={s3}
      submitLabel="Update"
      loadingText="Updating..."
    />
  );
}
