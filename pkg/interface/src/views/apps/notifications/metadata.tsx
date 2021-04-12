import React from 'react';
import { Box } from '@tlon/indigo-react';

import { MetadataBody, NotificationProps } from './types';
import { Header } from './header';

function getInvolvedUsers(body: MetadataBody) {
  return [];
}

function getDescription(body: MetadataBody) {
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

export function MetadataNotification(props: NotificationProps<'metadata'>) {
  const { unread } = props;
  const description = getDescription(unread.unreads[0].body);

  return (
    <Box p="2">
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
