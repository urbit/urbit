import React from 'react';
import { Col } from '@tlon/indigo-react';
import { MentionText } from '~/views/components/MentionText';
import useContactState from '~/logic/state/contact';


export function PostContent(props) {
  const { post, isParent, api, isReply } = props;
  const contacts = useContactState(state => state.contacts);

  return (
    <Col
      width="100%"
      pl="2"
      pr="2"
      pb={isParent || isReply ? "0" : "2"}
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

