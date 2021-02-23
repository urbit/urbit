import React from 'react';

import {
  Col,
  Label,
  BaseLabel,
  BaseAnchor
} from '@tlon/indigo-react';
import { GroupNotificationsConfig, Association, listenGroup, ignoreGroup } from '@urbit/api';

import GlobalApi from '~/logic/api/global';
import { StatelessAsyncToggle } from '~/views/components/StatelessAsyncToggle';
import useApi from '~/logic/lib/useApi';

export function GroupPersonalSettings(props: {
  association: Association;
  notificationsGroupConfig: GroupNotificationsConfig;
}) {
  const groupPath = props.association.group;
  const api = useApi();

  const watching = props.notificationsGroupConfig.findIndex(g => g === groupPath) !== -1;

  const onClick = async () => {
    if (!watching) {
      await api.poke(listenGroup(groupPath));
    } else {
      await api.poke(ignoreGroup(groupPath));
    }
  };

  return (
    <Col px="4" pb="4" gapY="4">
      <BaseAnchor pt="4" fontWeight="600" id="notifications" fontSize="2">Group Notifications</BaseAnchor>
      <BaseLabel
        htmlFor="asyncToggle"
        display="flex"
        cursor="pointer"
      >
        <StatelessAsyncToggle selected={watching} onClick={onClick} />
        <Col>
          <Label>Notify me on group activity</Label>
          <Label mt="2" gray>Send me notifications when this group changes</Label>
        </Col>
      </BaseLabel>
    </Col>
  );
}
