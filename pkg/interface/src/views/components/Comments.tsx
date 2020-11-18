import React from 'react';
import { Col } from '@tlon/indigo-react';
import { CommentItem } from './CommentItem';
import CommentInput from './CommentInput';
import { Contacts } from '~/types/contact-update';
import GlobalApi from '~/logic/api/global';
import { FormikHelpers } from 'formik';
import { GraphNode } from '~/types/graph-update';
import { createPost, createBlankNodeWithChildPost } from '~/logic/api/graph';
import { LocalUpdateRemoteContentPolicy } from '~/types';
import { scanForMentions } from '~/logic/lib/graph';

interface CommentsProps {
  comments: GraphNode;
  name: string;
  ship: string;
  contacts: Contacts;
  api: GlobalApi;
  hideAvatars: boolean;
  hideNicknames: boolean;
  remoteContentPolicy: LocalUpdateRemoteContentPolicy;
}

export function Comments(props: CommentsProps) {
  const { comments, ship, name, api } = props;

  const onSubmit = async (
    { comment },
    actions: FormikHelpers<{ comment: string }>
  ) => {
    try {
      const content = scanForMentions(comment);
      const node = createBlankNodeWithChildPost(
        comments?.post?.index,
        '1',
        content
      );
      await api.graph.addNode(ship, name, node);
      actions.resetForm();
      actions.setStatus({ success: null });
    } catch (e) {
      console.error(e);
      actions.setStatus({ error: e.message });
    }
  };

  return (
    <Col>
      <CommentInput onSubmit={onSubmit} />
      {Array.from(comments.children).reverse().map(([idx, comment]) => (
        <CommentItem
          comment={comment}
          key={idx.toString()}
          contacts={props.contacts}
          api={api}
          name={name}
          ship={ship}
          hideNicknames={props.hideNicknames}
          hideAvatars={props.hideAvatars}
          remoteContentPolicy={props.remoteContentPolicy}
        />
      ))}
    </Col>
  );
}

export default Comments;
