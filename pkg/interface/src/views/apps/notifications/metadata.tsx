import { Box } from '@tlon/indigo-react';
import { Metadata } from '@urbit/api';
import React from 'react';
import { Header } from './header';

function getDescription(body: { metadata: Metadata}) {
  const b = body.metadata;
  if ('new' in b) {
    return 'created';
  } else if ('changedTitle' in b) {
    return 'changed the title to';
  } else if ('changedDescription' in b) {
    return 'changed the description to';
  } else if ('changedColor' in b) {
    return 'changed the color to';
  } else if ('deleted' in b) {
    return 'deleted';
  } else {
    throw new Error('bad metadata frond');
  }
}

export function MetadataNotification(props: any) {
  const { unread } = props;
  const description = getDescription(unread.unreads[0].body);

  return (
    <Box p={2}>
      <Header
        authors={[]}
        description={description}
        group={unread.group}
        channel={unread.channel}
        date={unread.date}
      />
    </Box>
  );
}
