import React from 'react';
import { Col } from '@tlon/indigo-react';
import { MentionText } from '~/views/components/MentionText';


export function PostContent(props) {
  const { post, contacts } = props;
  return (
    <Col width="100%">
      <MentionText
        contacts={contacts}
        content={post.contents}
      />
    </Col>
  );
}

