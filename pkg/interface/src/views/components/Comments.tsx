import React from 'react';
import bigInt from 'big-integer';
import { Col } from '@tlon/indigo-react';
import { CommentItem } from './CommentItem';
import CommentInput from './CommentInput';
import { Contacts } from '~/types/contact-update';
import GlobalApi from '~/logic/api/global';
import { FormikHelpers } from 'formik';
import { GraphNode } from '~/types/graph-update';
import { createPost, createBlankNodeWithChildPost } from '~/logic/api/graph';
import { getLatestCommentRevision } from '~/logic/lib/publish';
import { LocalUpdateRemoteContentPolicy, Group } from '~/types';
import { scanForMentions } from '~/logic/lib/graph';

interface CommentsProps {
  comments: GraphNode;
  name: string;
  ship: string;
  editCommentId: string;
  baseUrl: string;
  contacts: Contacts;
  api: GlobalApi;
  hideAvatars: boolean;
  hideNicknames: boolean;
  remoteContentPolicy: LocalUpdateRemoteContentPolicy;
  group: Group;
}

export function Comments(props: CommentsProps) {
  const { comments, ship, name, api, baseUrl, history, group } = props;

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

  const onEdit = async (
    { comment },
    actions: FormikHelpers<{ comment: string }>
  ) => {
    try {
      const commentNode = comments.children.get(bigInt(props.editCommentId));
      const [idx, _] = getLatestCommentRevision(commentNode);

      const content = scanForMentions(comment);
      const post = createPost(
        content,
        commentNode.post.index,
        parseInt(idx + 1, 10)
      );
      await api.graph.addPost(ship, name, post);
      history.push(baseUrl);
    } catch (e) {
      console.error(e);
      actions.setStatus({ error: e.message });
    }
  };

  let commentContent = null;
  if (props.editCommentId) {
    const commentNode = comments.children.get(bigInt(props.editCommentId));
    const [_, post] = getLatestCommentRevision(commentNode);
    commentContent = post.contents.reduce((val, curr) => {
      if ('text' in curr) {
        val = val + curr.text;
      } else if ('mention' in curr) {
        val = val + curr.mention;
      } else if ('url' in curr) {
        val = val + curr.url;
      } else if ('code' in curr) {
        val = val + curr.code.expression;
      }
      return val;
    }, '');
  }

  return (
    <Col>
      {( !props.editCommentId ? <CommentInput onSubmit={onSubmit} /> : null )}
      {( !!props.editCommentId ? (
        <CommentInput
          onSubmit={onEdit}
          label='Edit Comment'
          loadingText='Editing...'
          initial={commentContent}
        />
      ) : null )}
      {Array.from(comments.children).reverse()
        .map(([idx, comment]) => {
          return (
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
              baseUrl={props.baseUrl}
              pending={idx.toString() === props.editCommentId}
            />
          );
      })}
    </Col>
  );
}

export default Comments;
