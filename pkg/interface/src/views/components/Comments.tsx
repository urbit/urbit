import React, { useEffect, useMemo } from 'react';
import bigInt from 'big-integer';
import { Col } from '@tlon/indigo-react';
import { CommentItem } from './CommentItem';
import CommentInput from './CommentInput';
import { Contacts } from '@urbit/api/contacts';
import GlobalApi from '~/logic/api/global';
import { FormikHelpers } from 'formik';
import { Group, GraphNode, Association } from '@urbit/api';
import { createPost, createBlankNodeWithChildPost } from '~/logic/api/graph';
import { getLatestCommentRevision } from '~/logic/lib/publish';
import tokenizeMessage from '~/logic/lib/tokenizeMessage';
import { getUnreadCount } from '~/logic/lib/hark';
import { PropFunc } from '~/types/util';
import { isWriter } from '~/logic/lib/group';
import useHarkState from '~/logic/state/hark';
import {useQuery} from '~/logic/lib/useQuery';
import {referenceToPermalink} from '~/logic/lib/permalinks';

interface CommentsProps {
  comments: GraphNode;
  association: Association;
  name: string;
  ship: string;
  editCommentId: string;
  baseUrl: string;
  api: GlobalApi;
  group: Group;
}

export function Comments(props: CommentsProps & PropFunc<typeof Col>) {
  const {
    association,
    comments,
    ship,
    name,
    api,
    history,
    baseUrl,
    group,
    ...rest
  } = props;

  const { query } = useQuery();
  const selectedComment = useMemo(() => {
    const id = query.get('selected')
    return id ? bigInt(id) : null;
  }, [query]);

  const editCommentId = useMemo(() => {
    const id = query.get('edit')
    return id || '';
  }, [query]);


  const onSubmit = async (
    { comment },
    actions: FormikHelpers<{ comment: string }>
  ) => {
    try {
      const content = tokenizeMessage(comment);
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
      const commentNode = comments.children.get(bigInt(editCommentId))!;
      const [idx, _] = getLatestCommentRevision(commentNode);

      const content = tokenizeMessage(comment);
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
      } else if ('reference' in curr) {
        val = `${val}web+urbit:/${referenceToPermalink(curr).link}`;
      }

      return val;
    }, '');
  }
  const parentIndex = `/${comments?.post.index.slice(1).split('/')[0]}`;

  const children = Array.from(comments.children);

  useEffect(() => {
    return () => {
      api.hark.markCountAsRead(association, parentIndex, 'comment');
    };
  }, [comments.post.index]);

  const unreads = useHarkState(state => state.unreads);
  const readCount = children.length - getUnreadCount(unreads, association.resource, parentIndex);

  const canComment = isWriter(group, association.resource) || association.metadata.vip === 'reader-comments';

  return (
    <Col {...rest}>
      {( !editCommentId && canComment ? <CommentInput onSubmit={onSubmit} /> : null )}
      {( editCommentId ? (
        <CommentInput
          onSubmit={onEdit}
          label='Edit Comment'
          loadingText='Editing...'
          initial={commentContent}
        />
      ) : null )}
      {children.reverse()
          .map(([idx, comment], i) => {
          const highlighted = selectedComment?.eq(idx) ?? false;
          return (
            <CommentItem
              highlighted={highlighted}
              comment={comment}
              key={idx.toString()}
              api={api}
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
