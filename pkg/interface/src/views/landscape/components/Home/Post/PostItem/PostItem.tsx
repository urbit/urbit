import { Box, Col, Row, Text } from '@tlon/indigo-react';
import { Association, GraphNode, Group, Post } from '@urbit/api';
import { BigInteger } from 'big-integer';
import { History } from 'history';
import React, { useCallback, useMemo, useState } from 'react';
import { useHistory } from 'react-router';
import { getPostRoute } from '~/logic/lib/graph';
import { isWriter } from '~/logic/lib/group';
import { useHovering } from '~/logic/lib/util';
import { Mention } from '~/views/components/MentionText';
import PostInput from '../PostInput';
import PostContent from './PostContent';
import PostFooter from './PostFooter';
import PostHeader from './PostHeader';

export interface PostItemProps {
  association: Association;
  baseUrl: string;
  bind?: unknown;
  graphPath: string;
  group: Group;
  history: History;
  hovering?: boolean;
  index: BigInteger[];
  isParent?: boolean;
  isRelativeTime?: boolean;
  isReply?: boolean;
  node: GraphNode;
  parentPost?: Post;
  vip: string;
  isThread?: boolean;
  isLast?: boolean;
  isHierarchical?: boolean;
}

function PostItem(props: PostItemProps) {
  const {
    node,
    group,
    association,
    index,
    vip,
    isHierarchical,
    isParent,
    isThread,
    isLast,
    isReply,
    isRelativeTime,
    parentPost
  } = props;

  const [inReplyMode, setInReplyMode] = useState(false);
  const toggleReplyMode = useCallback(() => setInReplyMode(m => !m), []);

  const history = useHistory();

  const canWrite = useMemo(() => {
    if (vip === '') {
      return true;
    }
    if (index && index.length > 0) {
      return true;
    }
    return isWriter(group, association.resource);
  }, [group, association.resource, vip, index]);

  const navigateToChildren = useCallback(() => {
    history.push(
      getPostRoute(association.resource, index, !isThread && !isHierarchical)
    );
  }, [
    isHierarchical,
    history.push,
    index,
    isParent,
    isThread,
    association.resource
  ]);

  const postExists = Boolean(node.post) && typeof node.post !== 'string';
  const { hovering, bind } = useHovering();

  return (
    <Col
      pl={1}
      pr={1}
      mb={isThread && !isLast && !inReplyMode ? 0 : 3}
      width="100%"
      alignItems="center"
    >
      <Col
        pt={2}
        border={1}
        borderColor={isParent ? 'gray' : 'lightGray'}
        borderRadius={2}
        width="100%"
        maxWidth="600px"
        backgroundColor={hovering ? 'washedGray' : 'transparent'}
        onClick={navigateToChildren}
        cursor={isParent ? 'default' : 'pointer'}
        {...bind}
      >
        {postExists ? (
          <>
            <PostHeader
              post={node.post}
              association={association}
              showTimestamp={isRelativeTime}
              graphPath={association.resource}
              isReply={isReply}
            />
            {(isReply || (parentPost && index.length > 1 && isParent)) &&
            parentPost?.author ? (
              <Row width="100%" alignItems="center" mb="2" pl="2" pr="2">
                <Text color="gray" pr="1">
                  Replying to
                </Text>
                <Mention ship={parentPost?.author} />
              </Row>
            ) : null}
            <PostContent
              post={node.post}
              isParent={isParent}
              isReply={isReply}
            />
            <PostFooter
              timeSent={node.post['time-sent']}
              replyCount={node.children.size}
              showTimestamp={!isRelativeTime}
              isParent={isParent}
              canComment={canWrite}
              toggleReplyMode={toggleReplyMode}
            />
          </>
        ) : (
          <Box px="2" pb="2">
            <Text gray>This post has been deleted</Text>
          </Box>
        )}
      </Col>
      {inReplyMode ? (
        <Col width="100%" maxWidth="600px">
          <Box
            ml={3}
            height="16px"
            borderLeft={1}
            borderLeftColor="lightGray"
          ></Box>
          <PostInput
            graphPath={association.resource}
            group={group}
            association={association}
            vip={vip}
            index={`/${index.join('/')}`}
            submitCallback={toggleReplyMode}
          />
        </Col>
      ) : null}
      {isThread && !isLast && !inReplyMode ? (
        <Col width="100%" maxWidth="600px">
          <Box
            ml={3}
            height="16px"
            borderLeft={1}
            borderLeftColor="lightGray"
          ></Box>
        </Col>
      ) : null}
    </Col>
  );
}

export default React.memo(PostItem);

