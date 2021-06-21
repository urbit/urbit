import { Col } from '@tlon/indigo-react';
import {
  createPost,
  createBlankNodeWithChildPost,
  Association,
  GraphNode,
  Group,
  markCountAsRead,
  addPost,
  resourceFromPath
} from '@urbit/api';
import bigInt from 'big-integer';
import { FormikHelpers } from 'formik';
import React, { useEffect, useMemo } from 'react';
import { isWriter } from '~/logic/lib/group';
import { getUnreadCount } from '~/logic/lib/hark';
import { referenceToPermalink } from '~/logic/lib/permalinks';
import { getLatestCommentRevision } from '~/logic/lib/publish';
import tokenizeMessage from '~/logic/lib/tokenizeMessage';
import { useQuery } from '~/logic/lib/useQuery';
import useHarkState from '~/logic/state/hark';
import { PropFunc } from '~/types/util';
import CommentInput from './CommentInput';
import { CommentItem } from './CommentItem';
import airlock from '~/logic/api';
import useGraphState from '~/logic/state/graph';
import { useHistory } from 'react-router';

interface CommentsProps {
  comments: GraphNode;
  association: Association;
  baseUrl: string;
  group: Group;
}

export function Comments(props: CommentsProps & PropFunc<typeof Col>) {
  const {
    association,
    comments,
    baseUrl,
    group,
    ...rest
  } = props;
  const addNode = useGraphState(s => s.addNode);
  const history = useHistory();

  const { ship, name } = resourceFromPath(association.resource);

  const { query } = useQuery();
  const selectedComment = useMemo(() => {
    const id = query.get('selected');
    return id ? bigInt(id) : null;
  }, [query]);

  const editCommentId = useMemo(() => {
    const id = query.get('edit');
    return id || '';
  }, [query]);

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
      addNode(ship, name, node);
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
      const [idx] = getLatestCommentRevision(commentNode);

      const content = tokenizeMessage(comment);
      const post = createPost(
        window.ship,
        content,
        commentNode.post.index,
        parseInt((idx + 1).toString(), 10).toString()
      );
      await airlock.thread(addPost(ship, name, post));
      history.push(baseUrl);
    } catch (e) {
      console.error(e);
      actions.setStatus({ error: e.message });
    }
  };

  let commentContent = null;
  if (editCommentId) {
    const commentNode = comments.children.get(bigInt(editCommentId));
    const [, post] = getLatestCommentRevision(commentNode);
    commentContent = post.contents.reduce((val, curr) => {
      if ('text' in curr) {
        val = val + curr.text;
      } else if ('mention' in curr) {
        val = val + `~${curr.mention}`;
      } else if ('url' in curr) {
        val = val + curr.url;
      } else if ('code' in curr) {
        val = val + curr.code.expression;
      } else if ('reference' in curr) {
        val = `${val}${referenceToPermalink(curr).link}`;
      }

      return val;
    }, '');
  }
  const parentIndex = `/${comments?.post.index.slice(1).split('/')[0]}`;

  const children = Array.from(comments.children);

  useEffect(() => {
    return () => {
      airlock.poke(markCountAsRead(association.resource));
    };
  }, [comments.post?.index]);

  const unreads = useHarkState(state => state.unreads);
  const readCount = children.length - getUnreadCount(unreads, association.resource, parentIndex);

  const canComment = isWriter(group, association.resource) || association.metadata.vip === 'reader-comments';

  return (
    <Col {...rest} minWidth={0}>
      {children.reverse()
          .map(([idx, comment], i) => {
          const highlighted = selectedComment?.eq(idx) ?? false;
          return (
            <CommentItem
              highlighted={highlighted}
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
      {( editCommentId ? (
        <CommentInput
          onSubmit={onEdit}
          label='Edit Comment'
          initial={commentContent}
        />
      ) : null )}
      {( !editCommentId && canComment ? <CommentInput placeholder="Comment" onSubmit={onSubmit} /> : null )}
    </Col>
  );
}

export default Comments;
