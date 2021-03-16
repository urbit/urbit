import React, { useEffect } from 'react';
import bigInt from 'big-integer';
import { Col } from '@tlon/indigo-react';
import { CommentItem } from './CommentItem';
import CommentInput from './CommentInput';
import { Contacts } from '@urbit/api/contacts';
import { FormikHelpers } from 'formik';
import { Group, GraphNode, Association, addPost, markCountAsRead, graph, hark, createBlankNodeWithChildPost, createPost, Patp, Post } from '@urbit/api';
import { getLatestCommentRevision } from '~/logic/lib/publish';
import tokenizeMessage from '~/logic/lib/tokenizeMessage';
import { getUnreadCount } from '~/logic/lib/hark';
import { PropFunc } from '~/types/util';
import { isWriter } from '~/logic/lib/group';
import useHarkState from '~/logic/state/hark';
import useApi from '~/logic/api';
import { addNode } from '@urbit/api/graph';
import useGraphState from '~/logic/state/graph';

interface CommentsProps {
  comments: GraphNode;
  association: Association;
  name: string;
  ship: string;
  editCommentId: string;
  baseUrl: string;
  group: Group;
}

export function Comments(props: CommentsProps & PropFunc<typeof Col>) {
  const {
    association,
    comments,
    ship,
    name,
    editCommentId,
    history,
    baseUrl,
    group,
    ...rest
  } = props;
  const api = useApi();
  const addNodes = useGraphState(state => state.addNodes);
  const addNode = (ship: Patp, name: string, node: GraphNode) => {
    const nodes: Record<string, GraphNode> = {};
    nodes[node.post.index] = node;
    return addNodes(ship, name, nodes);
  };
  const addPost = (ship: Patp, name: string, post: Post) => {
    const nodes: Record<string, GraphNode> = {};
    nodes[post.index] = {
      post,
      children: null
    };
    return addNodes(ship, name, nodes);
  }

  const onSubmit = async (
    { comment },
    actions: FormikHelpers<{ comment: string }>
  ) => {
    try {
      const content = tokenizeMessage(comment);
      const node = createBlankNodeWithChildPost(
        window.ship,
        comments?.post?.index,
        '1',
        content
      );
      await addNode(ship, name, node);
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
      const commentNode = comments.children.get(bigInt(editCommentId))!;
      const [idx, _] = getLatestCommentRevision(commentNode);

      const content = tokenizeMessage(comment);
      const post = createPost(
        window.ship,
        content,
        commentNode.post.index,
        parseInt(idx + 1, 10)
      );
      await addPost(ship, name, post);
      history.push(baseUrl);
    } catch (e) {
      console.error(e);
      actions.setStatus({ error: e.message });
    }
  };

  let commentContent = null;
  if (editCommentId) {
    const commentNode = comments.children.get(bigInt(editCommentId));
    const [_, post] = getLatestCommentRevision(commentNode);
    commentContent = post.contents.reduce((val, curr) => {
      if ('text' in curr) {
        val = val + curr.text;
      } else if ('mention' in curr) {
        val = val + `~${curr.mention}`;
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
    return () => {
      api.poke(hark.markCountAsRead(association, parentIndex, 'comment'));
    };
  }, [comments.post.index]);

  const unreads = useHarkState(state => state.unreads);
  const readCount = children.length - getUnreadCount(unreads, association.resource, parentIndex);

  const canComment = isWriter(group, association.resource) || association.metadata.vip === 'reader-comments';

  return (
    <Col {...rest}>
      {( !props.editCommentId && canComment ? <CommentInput onSubmit={onSubmit} /> : null )}
      {( props.editCommentId ? (
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
              
              name={name}
              ship={ship}
              unread={i >= readCount}
              baseUrl={props.baseUrl}
              group={group}
              pending={idx.toString() === editCommentId}
            />
          );
      })}
    </Col>
  );
}

export default Comments;
