import React from 'react';
import { Row, Icon } from '@tlon/indigo-react';
import Author from '~/views/components/Author';


export function PostHeader(props) {
  const { post, contacts, api, isReply } = props;
  const mb = isReply ? "2" : "3";

  return (
    <Row width="100%" height="36px" mb={mb} justifyContent="space-between">
      <Author
        showImage
        contacts={contacts}
        ship={post.author}
        date={post['time-sent']}
        unread={false}
        api={api}
        size={36}
        sigilPadding={8}
        time={true}
        showAsCol={true}
      />
      <Icon icon="Ellipsis" color="gray" />
    </Row>
  );
}

