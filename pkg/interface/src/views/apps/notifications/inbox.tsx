import React, { useEffect } from "react";
import f from "lodash/fp";
import _ from "lodash";
import { Icon, Col, Row, Box, Text, Anchor } from "@tlon/indigo-react";
import moment from "moment";
import { Notifications, Rolodex, Timebox, IndexedNotification } from "~/types";
import { MOMENT_CALENDAR_DATE, daToUnix } from "~/logic/lib/util";
import { BigInteger } from "big-integer";
import GlobalApi from "~/logic/api/global";
import { Notification } from "./notification";
import { Associations } from "~/types";

export default function Inbox(props: {
  notifications: Notifications;
  archive: Notifications;
  showArchive?: boolean;
  api: GlobalApi;
  associations: Associations;
  contacts: Rolodex;
  gr;
}) {
  const { api, associations } = props;
  useEffect(() => {
    let seen = false;
    setTimeout(() => {
      seen = true;
    }, 3000);
    return () => {
      if (seen) {
        //api.hark.seen();
      }
    };
  }, []);

  const [newNotifications, ...notifications] =
    Array.from(props.showArchive ? props.archive : props.notifications) || [];

  const inspect = f.tap((x: any) => {
    console.log(x);
  });

  const notificationsByDay = f.flow(
    f.groupBy<[BigInteger, Timebox]>(([date]) =>
      moment(daToUnix(date)).format("DDMMYYYY")
    ),
    f.values,
    inspect
  )(notifications);

  return (
    <Col overflowY="auto" flexGrow="1">
      {newNotifications && (
        <DaySection
          latest
          timeboxes={[newNotifications]}
          contacts={props.contacts}
          archive={!!props.showArchive}
          associations={props.associations}
          graphConfig={props.notificationsGraphConfig}
          groupConfig={props.notificationsGroupConfig}
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
              graphConfig={props.notificationsGraphConfig}
              groupConfig={props.notificationsGroupConfig}
            />
          )
      )}
    </Col>
  );
}

type DatedTimebox = [BigInteger, Timebox];

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
  archive,
  timeboxes,
  latest = false,
  associations,
  api,
  groupConfig,
  graphConfig,
}) {
  const calendar = latest
    ? MOMENT_CALENDAR_DATE
    : { ...MOMENT_CALENDAR_DATE, sameDay: "[Earlier Today]" };
  if (timeboxes.length === 0) {
    return null;
  }

  return (
    <>
      <Box position="sticky" zIndex="2" top="0px" bg="white">
        <Box p="2" bg="scales.black05">
          {moment(daToUnix(timeboxes[0][0])).calendar(null, calendar)}
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
              api={api}
              associations={associations}
              notification={not}
              archived={archive}
              contacts={contacts}
              time={date}
            />
          </React.Fragment>
        ))
      )}
    </>
  );
}
