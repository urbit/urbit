import React, { useRef } from 'react';

import { Col, Text, BaseLabel, Label } from '@tlon/indigo-react';
import { Association, ignoreGraph, listenGraph, NotificationGraphConfig } from '@urbit/api';

import { StatelessAsyncToggle } from '~/views/components/StatelessAsyncToggle';
import useApi from '~/logic/lib/useApi';

interface ChannelNotificationsProps {
  association: Association;
  notificationsGraphConfig: NotificationGraphConfig;
}

export function ChannelNotifications(props: ChannelNotificationsProps) {
  const { association } = props;
  const rid = association.resource;
  const api = useApi();

  const isMuted =
    props.notificationsGraphConfig.watching.findIndex(
      a => a.graph === rid && a.index === '/'
    ) === -1;

  const onChangeMute = async () => {
    if (isMuted) {
      await api.poke(listenGraph(rid, '/'));
    } else {
      await api.poke(ignoreGraph(rid, '/'));
    }
  };

  const anchorRef = useRef<HTMLElement | null>(null);

  return (
    <Col mb="6" gapY="4" flexShrink={0}>
      <Text ref={anchorRef} id="notifications" fontSize="2" fontWeight="bold">
        Channel Notifications
      </Text>
      <BaseLabel display="flex" cursor="pointer">
        <StatelessAsyncToggle selected={isMuted} onClick={onChangeMute} />
        <Col>
          <Label>Mute this channel</Label>
          <Label gray mt="1">
            Muting this channel will prevent it from sending updates to your
            inbox
          </Label>
        </Col>
      </BaseLabel>
    </Col>
  );
}
