import React, { ReactNode, useCallback, useMemo } from "react";
import { Row, Box, Col, Text, Anchor, Icon, Action } from "@tlon/indigo-react";
import _ from "lodash";
import {
  GraphNotificationContents,
  IndexedNotification,
  GroupNotificationContents,
  NotificationGraphConfig,
  GroupNotificationsConfig,
  NotifIndex,
  Associations
} from "~/types";
import GlobalApi from "~/logic/api/global";
import { StatelessAsyncAction } from "~/views/components/StatelessAsyncAction";
import { GroupNotification } from "./group";
import { GraphNotification } from "./graph";

interface NotificationProps {
  notification: IndexedNotification;
  time: BigInteger;
  associations: Associations;
  api: GlobalApi;
  archived: boolean;
  graphConfig: NotificationGraphConfig;
  groupConfig: GroupNotificationsConfig;
}

function getMuted(
  idx: NotifIndex,
  groups: GroupNotificationsConfig,
  graphs: NotificationGraphConfig
) {
  if ("graph" in idx) {
    const { graph } = idx.graph;
    return _.findIndex(graphs?.watching || [], (g) => g === graph) === -1;
  }
  if ("group" in idx) {
    return _.findIndex(groups || [], (g) => g === idx.group.group) === -1;
  }
  return false;
}

function NotificationWrapper(props: {
  api: GlobalApi;
  time: BigInteger;
  notif: IndexedNotification;
  children: ReactNode;
  archived: boolean;
  graphConfig: NotificationGraphConfig;
  groupConfig: GroupNotificationsConfig;
}) {
  const { api, time, notif, children } = props;

  const onArchive = useCallback(async () => {
    return api.hark.archive(time, notif.index);
  }, [time, notif]);

  const isMuted = getMuted(notif.index, props.groupConfig, props.graphConfig);

  const onChangeMute = useCallback(async () => {
    const func = isMuted ? "unmute" : "mute";
    return api.hark[func](notif.index);
  }, [notif, api, isMuted]);

  const changeMuteDesc = isMuted ? "Unmute" : "Mute";
  return (
    <Row alignItems="center" justifyContent="space-between">
      {children}
      <Row gapX="2" p="2" alignItems="center">
        <StatelessAsyncAction name={changeMuteDesc} onClick={onChangeMute}>
          {changeMuteDesc}
        </StatelessAsyncAction>
        {!props.archived && (
          <StatelessAsyncAction name={time.toString()} onClick={onArchive}>
            Archive
          </StatelessAsyncAction>
        )}
      </Row>
    </Row>
  );
}

export function Notification(props: NotificationProps) {
  const { notification, associations, archived } = props;
  const { read, contents, time } = notification.notification;

  if ("graph" in notification.index) {
    const index = notification.index.graph;
    const c: GraphNotificationContents = (contents as any).graph;

    return (
      <NotificationWrapper
        archived={archived}
        notif={notification}
        time={props.time}
        api={props.api}
        graphConfig={props.graphConfig}
        groupConfig={props.groupConfig}
      >
        <GraphNotification
          api={props.api}
          index={index}
          contents={c}
          contacts={props.contacts}
          read={read}
          archived={archived}
          timebox={props.time}
          time={time}
          associations={associations}
        />
      </NotificationWrapper>
    );
  }
  if ("group" in notification.index) {
    const index = notification.index.group;
    const c: GroupNotificationContents = (contents as any).group;
    return (
      <NotificationWrapper
        archived={archived}
        notif={notification}
        time={props.time}
        api={props.api}
        graphConfig={props.graphConfig}
        groupConfig={props.groupConfig}
      >
        <GroupNotification
          api={props.api}
          index={index}
          contents={c}
          contacts={props.contacts}
          read={read}
          timebox={props.time}
          archived={archived}
          time={time}
          associations={associations}
        />
      </NotificationWrapper>
    );
  }

  return null;
}
