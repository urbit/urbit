import React from 'react';
import { Box, Row } from '@tlon/indigo-react';
import { PostHeader } from './PostHeader';
import { PostContent } from './PostContent';
import { PostFooter } from './PostFooter';


class PostItem extends React.PureComponent {
  render() {
    const { index, node, groups, associations, api, contacts, innerRef } = this.props;
    return (
      <Row
        ref={innerRef}
        pl="1"
        pr="1"
        mb="3"
        width="100%"
        height="100px"
        justifyContent="center">
        <Box
          p="2"
          border={1}
          borderColor="washedGray"
          height="100px"
          width="100%"
          maxWidth="600px">
          <PostHeader node={node} contacts={contacts} />
          <PostContent {...this.props} />
          <PostFooter /> 
        </Box>
      </Row>
    );

  }
}

export default React.forwardRef(
  (props, ref) => <PostItem {...props} innerRef={ref} />
);

