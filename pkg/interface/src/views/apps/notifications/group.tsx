import React, { ReactElement, useCallback } from 'react';
import _ from 'lodash';

import { Col } from '@tlon/indigo-react';
import {
  Associations,
  GroupNotificationContents,
  GroupNotifIndex,
  GroupUpdate,
  Rolodex
} from '@urbit/api';

import { Header } from './header';
import GlobalApi from '~/logic/api/global';
import {useAssocForGroup} from '~/logic/state/metadata';

function describeNotification(description: string, plural: boolean) {
  switch (description) {
    case 'add-members':
      return 'joined';
    case 'remove-members':
      return 'left';
    default:
      return description;
  }
}

function getGroupUpdateParticipants(update: GroupUpdate): string[] {
  if ('addMembers' in update) {
    return update.addMembers.ships;
  }
  if ('removeMembers' in update) {
    return update.removeMembers.ships;
  }
  return [];
}

interface GroupNotificationProps {
  index: GroupNotifIndex;
  contents: GroupNotificationContents;
  archived: boolean;
  read: boolean;
  time: number;
  timebox: BigInteger;
  api: GlobalApi;
}

export function GroupNotification(props: GroupNotificationProps): ReactElement {
  const { contents, index, read, time, api, timebox } = props;

  const authors = _.flatten(_.map(contents, getGroupUpdateParticipants));

  const { group } = index;
  const desc = describeNotification(index.description, contents.length !== 1);

  const association = useAssocForGroup(group)
  const groupTitle = association?.metadata?.title ?? group;

  return (
    <Col>
      <Header
        time={time}
        authors={authors}
        description={desc}
        groupTitle={groupTitle}
      />
    </Col>
  );
}

