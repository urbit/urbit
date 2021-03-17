import React from 'react';
import { Row } from '@tlon/indigo-react';
import { MentionText } from '~/views/components/MentionText';


export function PostContent(props) {
  const { post, contacts } = props;
  return (
    <Row width="100%">
      <MentionText
        contacts={contacts}
        content={post.contents}
      />
    </Row>
  );
}

