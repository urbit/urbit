import { Col } from '@tlon/indigo-react';
import {
  GroupNotificationContents,
  GroupNotifIndex,
  GroupUpdate
} from '@urbit/api';
import bigInt from 'big-integer';
import _ from 'lodash';
import React, { ReactElement } from 'react';
import GlobalApi from '~/logic/api/global';
import { useAssocForGroup } from '~/logic/state/metadata';
import { Header } from './header';

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
  read: boolean;
  time: number;
  timebox: bigInt.BigInteger;
  api: GlobalApi;
}

export function GroupNotification(props: GroupNotificationProps): ReactElement {
  const { contents, index, time } = props;

  const authors = _.flatten(_.map(contents, getGroupUpdateParticipants));

  const { group } = index;
  const desc = describeNotification(index.description, contents.length !== 1);

  const association = useAssocForGroup(group);
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

