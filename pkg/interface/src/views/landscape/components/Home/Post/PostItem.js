import React from 'react';
import { Box, Col } from '@tlon/indigo-react';
import { PostHeader } from './PostHeader';
import { PostContent } from './PostContent';
import { PostFooter } from './PostFooter';
import { PostInput } from './PostInput';


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
    const { history, baseUrl, index } = this.props;
    //history.push(`${baseUrl}/feed/${index.toString()}`);
  }

  submitCallback() {
    this.toggleReplyMode();
    // or navigate to replies
  }

  render() {
    const { node, contacts, api, graphResource, index, innerRef } = this.props;
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
          borderColor="lightGray"
          borderRadius="2"
          width="100%"
          maxWidth="600px"
          onClick={this.navigateToReplies}>
          <PostHeader post={node.post} contacts={contacts} api={api} />
          <PostContent
            post={node.post}
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
              index={`/${index.toString()}`}
              submitCallback={this.submitCallback} />
          </Col>
        ) : null }
      </Col>
    );
  }
}

export default React.forwardRef(
  (props, ref) => <PostItem {...props} innerRef={ref} />
);

