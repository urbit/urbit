import {
    BaseLabel, Col,
    Label,

    Text
} from '@tlon/indigo-react';
import { Association, ignoreGroup, listenGroup } from '@urbit/api';
import React from 'react';
import useHarkState from '~/logic/state/hark';
import { StatelessAsyncToggle } from '~/views/components/StatelessAsyncToggle';
import airlock from '~/logic/api';

export function GroupPersonalSettings(props: {
  association: Association;
}) {
  const groupPath = props.association.group;

  const notificationsGroupConfig = useHarkState(state => state.notificationsGroupConfig);

  const watching = notificationsGroupConfig.findIndex(g => g === groupPath) !== -1;

  const onClick = async () => {
    const func = !watching ? listenGroup : ignoreGroup;
    await airlock.poke(func(groupPath));
  };

  return (
    <Col px={4} pb={4} gapY={4}>
      <Text pt={4} fontWeight="600" id="notifications" fontSize={2}>Group Notifications</Text>
      <BaseLabel
        htmlFor="asyncToggle"
        display="flex"
        cursor="pointer"
      >
        <StatelessAsyncToggle selected={watching} onClick={onClick} />
        <Col>
          <Label>Notify me on participant activity</Label>
          <Label mt={2} gray>When a user joins or leaves this group, send me a notification</Label>
        </Col>
      </BaseLabel>
    </Col>
  );
}
