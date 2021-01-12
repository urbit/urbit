import React, { useEffect } from 'react';
import bigInt from 'big-integer';
import { Col } from '@tlon/indigo-react';
import { CommentItem } from './CommentItem';
import CommentInput from './CommentInput';
import { Contacts } from '~/types/contact-update';
import GlobalApi from '~/logic/api/global';
import { FormikHelpers } from 'formik';
import { Group, GraphNode, Association } from '~/types';
import { createPost, createBlankNodeWithChildPost } from '~/logic/api/graph';
import { getLatestCommentRevision } from '~/logic/lib/publish';
import { scanForMentions } from '~/logic/lib/graph';
import { getUnreadCount } from '~/logic/lib/hark';

interface CommentsProps {
  comments: GraphNode;
  association: Association;
  name: string;
  ship: string;
  editCommentId: string;
  baseUrl: string;
  contacts: Contacts;
  api: GlobalApi;
  group: Group;
}

export function Comments(props: CommentsProps) {
  const { association, comments, ship, name, api, history, baseUrl, group } = props;

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
      const commentNode = comments.children.get(bigInt(props.editCommentId))!;
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
  const parentIndex = `/${comments?.post.index.slice(1).split('/')[0]}`;

  const children = Array.from(comments.children);


  useEffect(() => {
    console.log(`dismissing ${association?.['app-path']}`);
    return () => {
      api.hark.markCountAsRead(association, parentIndex, 'comment')
    };
  }, [comments.post.index])


  const readCount = children.length - getUnreadCount(props?.unreads, association['app-path'], parentIndex)

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
      {children.reverse()
        .map(([idx, comment], i) => {
          return (
            <CommentItem
              comment={comment}
              key={idx.toString()}
              contacts={props.contacts}
              api={api}
              name={name}
              ship={ship}
              unread={i >= readCount}
              baseUrl={props.baseUrl}
              group={group}
              pending={idx.toString() === props.editCommentId}
            />
          );
      })}
    </Col>
  );
}

export default Comments;
