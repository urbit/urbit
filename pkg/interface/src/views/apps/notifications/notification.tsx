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
  Groups,
  Associations,
  Contacts,
} from "~/types";
import GlobalApi from "~/logic/api/global";
import { getParentIndex } from "~/logic/lib/notification";
import { StatelessAsyncAction } from "~/views/components/StatelessAsyncAction";
import { GroupNotification } from "./group";
import { GraphNotification } from "./graph";
import { ChatNotification } from "./chat";
import { BigInteger } from "big-integer";

interface NotificationProps {
  notification: IndexedNotification;
  time: BigInteger;
  associations: Associations;
  api: GlobalApi;
  archived: boolean;
  groups: Groups;
  contacts: Contacts;
  graphConfig: NotificationGraphConfig;
  groupConfig: GroupNotificationsConfig;
  chatConfig: string[];
  remoteContentPolicy: any;
}

function getMuted(
  idxNotif: IndexedNotification,
  groups: GroupNotificationsConfig,
  graphs: NotificationGraphConfig,
  chat: string[]
) {
  const { index, notification } = idxNotif;
  if ("graph" in idxNotif.index) {
    const { graph } = idxNotif.index.graph;
    if(!('graph' in notification.contents)) {
      throw new Error();
    }
    const parent = getParentIndex(index.graph, notification.contents.graph);

    return _.findIndex(
      graphs?.watching || [],
      (g) => g.graph === graph && g.index === parent
    ) === -1;
  }
  if ("group" in index) {
    return _.findIndex(groups || [], (g) => g === index.group.group) === -1;
  }
  if ("chat" in index) {
    return _.findIndex(chat || [], (c) => c === index.chat.chat) === -1;
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
  chatConfig: string[];
}) {
  const { api, time, notif, children } = props;

  const onArchive = useCallback(async () => {
    return api.hark.archive(time, notif.index);
  }, [time, notif]);

  const isMuted = getMuted(
    notif,
    props.groupConfig,
    props.graphConfig,
    props.chatConfig
  );

  const onChangeMute = useCallback(async () => {
    const func = isMuted ? "unmute" : "mute";
    return api.hark[func](notif);
  }, [notif, api, isMuted]);

  const changeMuteDesc = isMuted ? "Unmute" : "Mute";
  return (
    <Row flexShrink={0} alignItems="top" justifyContent="space-between">
      {children}
      <Row gapX="2" p="2" pt='3' alignItems="top">
        <StatelessAsyncAction name={changeMuteDesc} onClick={onChangeMute} backgroundColor="transparent">
          {changeMuteDesc}
        </StatelessAsyncAction>
        {!props.archived && (
          <StatelessAsyncAction name={time.toString()} onClick={onArchive} backgroundColor="transparent">
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

  const Wrapper = ({ children }) => (
    <NotificationWrapper
      archived={archived}
      notif={notification}
      time={props.time}
      api={props.api}
      graphConfig={props.graphConfig}
      groupConfig={props.groupConfig}
      chatConfig={props.chatConfig}
    >
      {children}
    </NotificationWrapper>
  );

  if ("graph" in notification.index) {
    const index = notification.index.graph;
    const c: GraphNotificationContents = (contents as any).graph;

    return (
      <Wrapper>
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
          remoteContentPolicy={props.remoteContentPolicy}
        />
      </Wrapper>
    );
  }
  if ("group" in notification.index) {
    const index = notification.index.group;
    const c: GroupNotificationContents = (contents as any).group;
    return (
      <Wrapper>
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
      </Wrapper>
    );
  }
  if ("chat" in notification.index) {
    const index = notification.index.chat;
    const c: ChatNotificationContents = (contents as any).chat;
    return (
      <Wrapper>
        <ChatNotification
          api={props.api}
          index={index}
          contents={c}
          contacts={props.contacts}
          read={read}
          archived={archived}
          groups={props.groups}
          timebox={props.time}
          time={time}
          associations={associations}
          remoteContentPolicy={props.remoteContentPolicy}
        />
      </Wrapper>
    );
  }

  return null;
}
