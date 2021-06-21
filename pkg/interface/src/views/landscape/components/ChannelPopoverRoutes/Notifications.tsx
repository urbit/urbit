import { BaseLabel, Col, Label, Text } from '@tlon/indigo-react';
import { Association, ignoreGraph, listenGraph } from '@urbit/api';
import React, { useRef } from 'react';
import useHarkState from '~/logic/state/hark';
import { StatelessAsyncToggle } from '~/views/components/StatelessAsyncToggle';
import airlock from '~/logic/api';

interface ChannelNotificationsProps {
  association: Association;
}

export function ChannelNotifications(props: ChannelNotificationsProps) {
  const { association } = props;
  const rid = association.resource;
  const notificationsGraphConfig = useHarkState(state => state.notificationsGraphConfig);

  const isMuted =
    notificationsGraphConfig.watching.findIndex(
      a => a.graph === rid && a.index === '/'
    ) === -1;

  const onChangeMute = async () => {
    const func = isMuted ? listenGraph : ignoreGraph;
    await airlock.poke(func(rid, '/'));
  };

  const anchorRef = useRef<HTMLElement | null>(null);

  return (
    <Col mx={4} mb={6} gapY={4} flexShrink={0}>
      <Text ref={anchorRef} id="notifications" fontSize={2} fontWeight="bold">
        Channel Notifications
      </Text>
      <BaseLabel display="flex" cursor="pointer">
        <StatelessAsyncToggle selected={isMuted} onClick={onChangeMute} />
        <Col>
          <Label>Mute this channel</Label>
          <Label gray mt={1}>
            Muting this channel will prevent it from sending updates to your
            inbox
          </Label>
        </Col>
      </BaseLabel>
    </Col>
  );
}
