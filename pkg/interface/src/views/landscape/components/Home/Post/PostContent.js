import React from 'react';
import { Col } from '@tlon/indigo-react';
import { MentionText } from '~/views/components/MentionText';


export function PostContent(props) {
  const { post, contacts, isParent } = props;
  return (
    <Col
      width="100%"
      maxHeight={ isParent ? "none" : "300px" }
      textOverflow="ellipsis"
      overflow="hidden"
      display="inline-block">
      <MentionText
        contacts={contacts}
        content={post.contents}
      />
    </Col>
  );
}

