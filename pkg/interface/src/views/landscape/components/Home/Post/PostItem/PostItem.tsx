import { Box, Col, Row, Text } from '@tlon/indigo-react';
import { Association, GraphNode, Group, Post } from '@urbit/api';
import { History } from 'history';
import React, { Ref } from 'react';
import GlobalApi from '~/logic/api/global';
import { isWriter } from '~/logic/lib/group';
import { withHovering } from '~/logic/lib/util';
import { Mention } from '~/views/components/MentionText';
import PostInput from '../PostInput';
import PostContent from './PostContent';
import PostFooter from './PostFooter';
import PostHeader from './PostHeader';

export interface PostItemProps {
  api: GlobalApi;
  association: Association;
  baseUrl: string
  bind?: unknown;
  graphPath: string;
  group: Group;
  history: History;
  hovering?: boolean;
  index: BigInt[];
  innerRef?: Ref<HTMLDivElement>;
  isParent?: boolean;
  isRelativeTime?: boolean;
  isReply?: boolean;
  node: GraphNode;
  parentPost?: Post;
  vip: string;
  isThread?: boolean;
  isLast?: boolean;
}

interface PostItemState {
  inReplyMode: boolean;
}

class PostItem extends React.Component<PostItemProps, PostItemState> {
  constructor(props) {
    super(props);

    this.state = { inReplyMode: false };
    this.toggleReplyMode = this.toggleReplyMode.bind(this);
    this.navigateToChildren = this.navigateToChildren.bind(this);
    this.submitCallback = this.submitCallback.bind(this);
  }

  canWrite() {
    const {
      group,
      association,
      vip,
      index
    } = this.props;

    if (vip === '') {
      return true;
    }

    if (index && index.length > 0) {
      return true;
    }

    return isWriter(group, association.resource);
  }

  toggleReplyMode() {
    this.setState({ inReplyMode: !this.state.inReplyMode });
  }

  navigateToChildren() {
    const { history, baseUrl, index, isParent, isThread } = this.props;
    if (isParent) {
      return;
    }
    let indexString = '';

    index.forEach((i) => {
      indexString = indexString + '/' + i.toString();
    });

    //  TODO: ensure that the logic here works properly
    if (!isThread) {
      history.push(`${baseUrl}/feed/thread${indexString}`);
    } else {
      history.push(`${baseUrl}/feed/replies${indexString}`);
    }
  }

  submitCallback() {
    this.toggleReplyMode();
  }

  render() {
    const {
      node,
      api,
      graphPath,
      association,
      index,
      innerRef,
      isParent = false,
      isReply = false,
      isRelativeTime = true,
      parentPost,
      vip,
      group,
      hovering,
      bind,
      isThread,
      isLast
    } = this.props;

    let indexString = '';

    index.forEach((i) => {
      indexString = indexString + '/' + i.toString();
    });

    const { inReplyMode } = this.state;

    const canComment = this.canWrite();
    const postExists = !!node.post && typeof node.post !== 'string';

    return (
      <Col
        ref={innerRef}
        pl={1}
        pr={1}
        mb={isThread && !isLast && !inReplyMode ? 0 : 3}
        width="100%"
        alignItems="center"
      >
        <Col
          pt={2}
          border={1}
          borderColor={ isParent ? 'gray' : 'lightGray' }
          borderRadius={2}
          width="100%"
          maxWidth="600px"
          backgroundColor={ hovering ? 'washedGray' : 'transparent' }
          onClick={this.navigateToChildren}
          cursor={isParent ? "default": "pointer"}
          {...bind}>
          { (postExists) ? (
            <>
              <PostHeader
                post={node.post}
                api={api}
                association={association}
                showTimestamp={isRelativeTime}
                graphPath={graphPath}
                isReply={isReply} />
              { (isReply || (parentPost && index.length > 1 && isParent)) ? (
                <Row width="100%" alignItems="center" mb="2" pl="2" pr="2">
                  <Text color="gray" pr="1">Replying to</Text>
                  <Mention ship={parentPost?.author} api={api} />
                </Row>
              ) : null }
              <PostContent
                post={node.post}
                isParent={isParent}
                isReply={isReply}
                api={api} />
            <PostFooter
              timeSent={node.post['time-sent']}
              replyCount={node.children.size}
              showTimestamp={!isRelativeTime}
              isParent={isParent}
              canComment={canComment}
              toggleReplyMode={this.toggleReplyMode} />
            </>
          ) : (
            <Box px="2" pb="2">
              <Text gray>This post has been deleted</Text>
            </Box>
          ) }
        </Col>
        { inReplyMode ? (
          <Col width="100%" maxWidth="600px">
            <Box
              ml={3}
              height="16px"
              borderLeft={1}
              borderLeftColor="lightGray"
            ></Box>
            <PostInput
              api={api}
              graphPath={graphPath}
              group={group}
              association={association}
              vip={vip}
              index={indexString}
              submitCallback={this.submitCallback}
            />
          </Col>
        ) : null }
      { isThread && !isLast && !inReplyMode ? (
          <Col width="100%" maxWidth="600px">
            <Box
              ml={3}
              height="16px"
              borderLeft={1}
              borderLeftColor="lightGray"
            ></Box>
          </Col>
        ) : null }
      </Col>
    );
  }
}

export default withHovering<PostItemProps>(PostItem);
