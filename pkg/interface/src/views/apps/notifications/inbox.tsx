import React, { useEffect, useCallback } from "react";
import f from "lodash/fp";
import _ from "lodash";
import { Icon, Col, Row, Box, Text, Anchor } from "@tlon/indigo-react";
import moment from "moment";
import { Notifications, Rolodex, Timebox, IndexedNotification, Groups } from "~/types";
import { MOMENT_CALENDAR_DATE, daToUnix, resourceAsPath } from "~/logic/lib/util";
import { BigInteger } from "big-integer";
import GlobalApi from "~/logic/api/global";
import { Notification } from "./notification";
import { Associations } from "~/types";
import { cite } from '~/logic/lib/util';
import { InviteItem } from '~/views/components/Invite';
import { useWaitForProps } from "~/logic/lib/useWaitForProps";
import { useHistory } from "react-router-dom";

type DatedTimebox = [BigInteger, Timebox];

function filterNotification(associations: Associations, groups: string[]) {
  if (groups.length === 0) {
    return () => true;
  }
  return (n: IndexedNotification) => {
    if ("graph" in n.index) {
      const { group } = n.index.graph;
      return groups.findIndex((g) => group === g) !== -1;
    } else if ("group" in n.index) {
      const { group } = n.index.group;
      return groups.findIndex((g) => group === g) !== -1;
    } else if ("chat" in n.index) {
      const group = associations.chat[n.index.chat.chat]?.["group-path"];
      return groups.findIndex((g) => group === g) !== -1;
    }
    return true;
  };
}

export default function Inbox(props: {
  notifications: Notifications;
  archive: Notifications;
  groups: Groups;
  showArchive?: boolean;
  api: GlobalApi;
  associations: Associations;
  contacts: Rolodex;
  filter: string[];
  invites: any;
}) {
  const { api, associations, invites } = props;
  const waiter = useWaitForProps(props)
  const history = useHistory();
  useEffect(() => {
    let seen = false;
    setTimeout(() => {
      seen = true;
    }, 3000);
    return () => {
      if (seen) {
        api.hark.seen();
      }
    };
  }, []);

  const [newNotifications, ...notifications] =
    Array.from(props.showArchive ? props.archive : props.notifications) || [];

  const notificationsByDay = f.flow(
    f.map<DatedTimebox>(([date, nots]) => [
      date,
      nots.filter(filterNotification(associations, props.filter)),
    ]),
    f.groupBy<DatedTimebox>(([date]) =>
      moment(daToUnix(date)).format("DDMMYYYY")
    ),
    f.values,
    f.reverse
  )(notifications);

  useEffect(() => {
    api.hark.getMore(props.showArchive);
  }, [props.showArchive]);

  const onScroll = useCallback((e) => {
    let container = e.target;
    const { scrollHeight, scrollTop, clientHeight } = container;
    if((scrollHeight - scrollTop) < 1.5 * clientHeight) {
      api.hark.getMore(props.showArchive);
    }
  }, [props.showArchive]);

  const acceptInvite = (app: string, uid: string) => async (invite) => {
    const resource = {
      ship: `~${invite.resource.ship}`,
      name: invite.resource.name
    };

    const resourcePath = resourceAsPath(invite.resource);
    if(app === 'contacts') {
      await api.contacts.join(resource);
      await waiter(p => resourcePath in p.associations?.contacts);
      await api.invite.accept(app, uid);
      history.push(`/~landscape${resourcePath}`);
    } else if ( app === 'chat') {
      await api.invite.accept(app, uid);
      history.push(`/~landscape/home/resource/chat${resourcePath.slice(5)}`);
    } else if ( app === 'graph') {
      await api.invite.accept(app, uid);
      history.push(`/~graph/join${resourcePath}`);
    }
  };

  const inviteItems = (invites, api) => {
    const returned = [];
    Object.keys(invites).map((appKey) => {
      const app = invites[appKey];
      Object.keys(app).map((uid) => {
        const invite = app[uid];
        const inviteItem =
          <InviteItem
            key={uid}
            invite={invite}
            onAccept={acceptInvite(appKey, uid)}
            onDecline={() => api.invite.decline(appKey, uid)}
          />;
        returned.push(inviteItem);
      });
    });
    return returned;
  };

  return (
    <Col height="100%" overflowY="auto" onScroll={onScroll} >
      {inviteItems(invites, api)}
      {newNotifications && (
        <DaySection
          latest
          timeboxes={[newNotifications]}
          contacts={props.contacts}
          archive={!!props.showArchive}
          associations={props.associations}
          groups={props.groups}
          graphConfig={props.notificationsGraphConfig}
          groupConfig={props.notificationsGroupConfig}
          chatConfig={props.notificationsChatConfig}
          remoteContentPolicy={props.remoteContentPolicy}
          api={api}
        />
      )}

      {_.map(
        notificationsByDay,
        (timeboxes, idx) =>
          timeboxes.length > 0 && (
            <DaySection
              key={idx}
              timeboxes={timeboxes}
              contacts={props.contacts}
              archive={!!props.showArchive}
              associations={props.associations}
              api={api}
              groups={props.groups}
              graphConfig={props.notificationsGraphConfig}
              groupConfig={props.notificationsGroupConfig}
              chatConfig={props.notificationsChatConfig}
              remoteContentPolicy={props.remoteContentPolicy}
            />
          )
      )}
    </Col>
  );
}

function sortTimeboxes([a]: DatedTimebox, [b]: DatedTimebox) {
  return b.subtract(a);
}

function sortIndexedNotification(
  { notification: a }: IndexedNotification,
  { notification: b }: IndexedNotification
) {
  return b.time - a.time;
}

function DaySection({
  contacts,
  groups,
  archive,
  timeboxes,
  latest = false,
  associations,
  api,
  groupConfig,
  graphConfig,
  chatConfig,
  remoteContentPolicy
}) {
  const calendar = latest
    ? MOMENT_CALENDAR_DATE
    : { ...MOMENT_CALENDAR_DATE, sameDay: "[Earlier Today]" };
  const lent = timeboxes.map(([,nots]) => nots.length).reduce(f.add, 0);
  if (lent === 0 || timeboxes.length === 0) {
    return null;
  }

  return (
    <>
      <Box position="sticky" zIndex="3" top="-1px" bg="white">
        <Box p="2" bg="scales.black05">
          <Text>
            {moment(daToUnix(timeboxes[0][0])).calendar(null, calendar)}
          </Text>
        </Box>
      </Box>
      {_.map(timeboxes.sort(sortTimeboxes), ([date, nots], i) =>
        _.map(nots.sort(sortIndexedNotification), (not, j: number) => (
          <React.Fragment key={j}>
            {(i !== 0 || j !== 0) && (
              <Box flexShrink="0" height="4px" bg="scales.black05" />
            )}
            <Notification
              graphConfig={graphConfig}
              groupConfig={groupConfig}
              chatConfig={chatConfig}
              api={api}
              associations={associations}
              notification={not}
              archived={archive}
              contacts={contacts}
              groups={groups}
              time={date}
              remoteContentPolicy={remoteContentPolicy}
            />
          </React.Fragment>
        ))
      )}
    </>
  );
}
