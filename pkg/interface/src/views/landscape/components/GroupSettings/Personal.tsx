import React from 'react';

import {
  Col,
  Label,
  BaseLabel,
  Text
} from '@tlon/indigo-react';
import { GroupNotificationsConfig } from '@urbit/api';
import { Association } from '@urbit/api/metadata';

import GlobalApi from '~/logic/api/global';
import { StatelessAsyncToggle } from '~/views/components/StatelessAsyncToggle';
import useHarkState from '~/logic/state/hark';

export function GroupPersonalSettings(props: {
  api: GlobalApi;
  association: Association;
}) {
  const groupPath = props.association.group;

  const notificationsGroupConfig = useHarkState(state => state.notificationsGroupConfig);

  const watching = notificationsGroupConfig.findIndex(g => g === groupPath) !== -1;

  const onClick = async () => {
    const func = !watching ? 'listenGroup' : 'ignoreGroup';
    await props.api.hark[func](groupPath);
  };

  return (
    <Col px="4" pb="4" gapY="4">
      <Text pt="4" fontWeight="600" id="notifications" fontSize="2">Group Notifications</Text>
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
