import React, { ReactNode, useCallback, useMemo, useState } from "react";
import { Row, Box } from "@tlon/indigo-react";
import _ from "lodash";
import {
  GraphNotificationContents,
  IndexedNotification,
  GroupNotificationContents,
  NotificationGraphConfig,
  GroupNotificationsConfig,
  Groups,
  Associations,
  Contacts,
} from "@urbit/api";
import GlobalApi from "~/logic/api/global";
import { getParentIndex } from "~/logic/lib/notification";
import { StatelessAsyncAction } from "~/views/components/StatelessAsyncAction";
import { GroupNotification } from "./group";
import { GraphNotification } from "./graph";
import { BigInteger } from "big-integer";
import { useHovering } from "~/logic/lib/util";
import useHarkState from "~/logic/state/hark";

interface NotificationProps {
  notification: IndexedNotification;
  time: BigInteger;
  api: GlobalApi;
  archived: boolean;
}

function getMuted(
  idxNotif: IndexedNotification,
  groups: GroupNotificationsConfig,
  graphs: NotificationGraphConfig
) {
  const { index, notification } = idxNotif;
  if ("graph" in idxNotif.index) {
    const { graph } = idxNotif.index.graph;
    if (!("graph" in notification.contents)) {
      throw new Error();
    }
    const parent = getParentIndex(index.graph, notification.contents.graph);

    return (
      _.findIndex(
        graphs?.watching || [],
        (g) => g.graph === graph && g.index === parent
      ) === -1
    );
  }
  if ("group" in index) {
    return _.findIndex(groups || [], (g) => g === index.group.group) === -1;
  }
  return false;
}

export function NotificationWrapper(props: {
  api: GlobalApi;
  time?: BigInteger;
  notification?: IndexedNotification;
  children: ReactNode;
}) {
  const { api, time, notification, children } = props;

  const onArchive = useCallback(async () => {
    if (!(time && notification)) {
      return;
    }
    return api.hark.archive(time, notification.index);
  }, [time, notification]);

  const groupConfig = useHarkState((state) => state.notificationsGroupConfig);
  const graphConfig = useHarkState((state) => state.notificationsGraphConfig);

  const isMuted =
    time && notification && getMuted(notification, groupConfig, graphConfig);

  const onChangeMute = useCallback(async () => {
    if (!notification) {
      return;
    }
    const func = isMuted ? "unmute" : "mute";
    return api.hark[func](notification);
  }, [notification, api, isMuted]);

  const onClick = () => {
    if (!(time && notification) || notification.notification.read) {
      return;
    }
    return api.hark.read(time, notification.index);
  };

  const { hovering, bind } = useHovering();

  const changeMuteDesc = isMuted ? "Unmute" : "Mute";
  return (
    <Box
      onClick={onClick}
      bg={
        (notification ? notification?.notification?.read : false)
          ? "washedGray"
          : "washedBlue"
      }
      borderRadius={2}
      display="grid"
      gridTemplateColumns={["1fr", "1fr 200px"]}
      gridTemplateRows="auto"
      gridTemplateAreas={["'header' 'main'", "'header actions' 'main main'"]}
      p={2}
      m={2}
      {...bind}
    >
      {children}
      <Row
        display={["none", "flex"]}
        alignItems="center"
        gapX="2"
        gridArea="actions"
        justifyContent="flex-end"
        opacity={[1, hovering ? 1 : 0]}
      >
        {time && notification && (
          <>
            <StatelessAsyncAction
              name={changeMuteDesc}
              onClick={onChangeMute}
              backgroundColor="transparent"
            >
              {changeMuteDesc}
            </StatelessAsyncAction>
            <StatelessAsyncAction
              name={time.toString()}
              onClick={onArchive}
              backgroundColor="transparent"
            >
              Dismiss
            </StatelessAsyncAction>
          </>
        )}
      </Row>
    </Box>
  );
}

export function Notification(props: NotificationProps) {
  const { notification, associations, archived } = props;
  const { read, contents, time } = notification.notification;

  const wrapperProps = {
    notification,
    time: props.time,
    api: props.api,
  };

  if ("graph" in notification.index) {
    const index = notification.index.graph;
    const c: GraphNotificationContents = (contents as any).graph;

    return (
      <NotificationWrapper {...wrapperProps}>
        <GraphNotification
          api={props.api}
          index={index}
          contents={c}
          read={read}
          archived={archived}
          timebox={props.time}
          time={time}
        />
      </NotificationWrapper>
    );
  }
  if ("group" in notification.index) {
    const index = notification.index.group;
    const c: GroupNotificationContents = (contents as any).group;
    return (
      <NotificationWrapper {...wrapperProps}>
        <GroupNotification
          api={props.api}
          index={index}
          contents={c}
          read={read}
          timebox={props.time}
          archived={archived}
          time={time}
        />
      </NotificationWrapper>
    );
  }

  return null;
}
