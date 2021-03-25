import React from 'react';
import { Box, Col, Row, Text } from '@tlon/indigo-react';
import { PostHeader } from './PostHeader';
import { PostContent } from './PostContent';
import { PostFooter } from './PostFooter';
import { PostInput } from './PostInput';
import { Mention } from "~/views/components/MentionText";
import withState from '~/logic/lib/withState';
import { useHovering } from '~/logic/lib/util';


class PostItem extends React.Component {
  
  constructor(props) {
    super(props);

    this.state = { inReplyMode: false };
    this.toggleReplyMode = this.toggleReplyMode.bind(this);
    this.navigateToReplies = this.navigateToReplies.bind(this);
    this.submitCallback = this.submitCallback.bind(this);
  }

  toggleReplyMode() {
    this.setState({ inReplyMode: !this.state.inReplyMode });
  }

  navigateToReplies() {
    const { history, baseUrl, index, isParent } = this.props;
    if (isParent) { return; }
    let indexString = '';
    
    index.forEach((i) => {
      indexString = indexString + '/' + i.toString();
    });

    history.push(`${baseUrl}/feed${indexString}`);
  }

  submitCallback() {
    this.toggleReplyMode();
  }

  render() {
    const {
      node,
      contacts,
      api,
      graphResource,
      index,
      innerRef,
      isParent,
      isReply,
      parentPost,
      hovering,
      bind
    } = this.props;

    let indexString = '';
    
    index.forEach((i) => {
      indexString = indexString + '/' + i.toString();
    });

    const { inReplyMode } = this.state;

    return (
      <Col
        ref={innerRef}
        pl="1"
        pr="1"
        mb="3"
        width="100%"
        alignItems="center">
        <Col
          p="2"
          border={1}
          borderColor={ isParent ? "gray" : "lightGray" }
          borderRadius="2"
          width="100%"
          maxWidth="600px"
          backgroundColor={ hovering ? 'washedGray' : 'transparent' }
          onClick={this.navigateToReplies}
          cursor={isParent ? "default": "pointer"}
          {...bind}>
          <PostHeader post={node.post} contacts={contacts} api={api} isReply={isReply} />
          { isReply ? (
            <Row width="100%" alignItems="center" mb="3">
              <Text color="gray" pr="1">Replying to</Text>
              <Mention ship={parentPost.author} />
            </Row>
          ) : null }
          <PostContent
            post={node.post}
            isParent={isParent}
            contacts={contacts} />
          <PostFooter
            replyCount={node.children.size}
            toggleReplyMode={this.toggleReplyMode} /> 
        </Col>
        { inReplyMode ? (
          <Col width="100%" maxWidth="600px">
            <Box
              ml="3"
              height="16px"
              borderLeft={1}
              borderLeftColor="lightGray"></Box>
            <PostInput
              api={api}
              graphResource={graphResource}
              index={indexString}
              submitCallback={this.submitCallback} />
          </Col>
        ) : null }
      </Col>
    );
  }
}


export default withState(PostItem, [
  [useHovering, ['hovering', 'bind']],
]);

