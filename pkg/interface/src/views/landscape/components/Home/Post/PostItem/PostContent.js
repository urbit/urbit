import React from 'react';
import { Col } from '@tlon/indigo-react';
import { MentionText } from '~/views/components/MentionText';
import useContactState from '~/logic/state/contact';


export function PostContent(props) {
  const { post, isParent, api } = props;
  const contacts = useContactState(state => state.contacts);

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
        api={api}
        transcluded={0}
      />
    </Col>
  );
}

