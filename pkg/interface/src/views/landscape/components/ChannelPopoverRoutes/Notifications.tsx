import React, { useRef } from 'react';
import { Col, Text, BaseLabel, Label } from '@tlon/indigo-react';
import GlobalApi from '~/logic/api/global';
import { Association, NotificationGraphConfig } from '@urbit/api';
import { StatelessAsyncToggle } from '~/views/components/StatelessAsyncToggle';
import useHarkState from '~/logic/state/hark';

interface ChannelNotificationsProps {
  api: GlobalApi;
  association: Association;
}

export function ChannelNotifications(props: ChannelNotificationsProps) {
  const { api, association } = props;
  const rid = association.resource;
  const notificationsGraphConfig = useHarkState(state => state.notificationsGraphConfig);

  const isMuted =
    notificationsGraphConfig.watching.findIndex(
      a => a.graph === rid && a.index === '/'
    ) === -1;

  const onChangeMute = async () => {
    const func = isMuted ? 'listenGraph' : 'ignoreGraph';
    await api.hark[func](rid, '/');
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
