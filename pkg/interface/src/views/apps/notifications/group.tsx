import React, { ReactNode, useCallback } from "react";
import moment from "moment";
import { Row, Box, Col, Text, Anchor, Icon, Action } from "@tlon/indigo-react";
import _ from "lodash";
import { NotificationProps } from "./types";
import {
  Post,
  GraphNotifIndex,
  GraphNotificationContents,
  Associations,
  Content,
  IndexedNotification,
  GroupNotificationContents,
  GroupNotifIndex,
  GroupUpdate,
  Rolodex,
} from "~/types";
import { Header } from "./header";
import { cite, deSig } from "~/logic/lib/util";
import { Sigil } from "~/logic/lib/sigil";
import RichText from "~/views/components/RichText";
import GlobalApi from "~/logic/api/global";
import { StatelessAsyncAction } from "~/views/components/StatelessAsyncAction";


function describeNotification(description: string, plural: boolean) {
  switch (description) {
    case "add-members":
      return "joined";
    case "remove-members":
      return "left";
    default:
      return description;
  }
}

function getGroupUpdateParticipants(update: GroupUpdate) {
  if ("addMembers" in update) {
    return update.addMembers.ships;
  }
  if ("removeMembers" in update) {
    return update.removeMembers.ships;
  }
  return [];
}

interface GroupNotificationProps {
  index: GroupNotifIndex;
  contents: GroupNotificationContents;
  archived: boolean;
  read: boolean;
  time: number;
  timebox: BigInteger;
  associations: Associations;
  contacts: Rolodex;
  api: GlobalApi;
}

export function GroupNotification(props: GroupNotificationProps) {
  const { contents, index, read, time, api, timebox, associations } = props;

  const authors = _.flatten(_.map(contents, getGroupUpdateParticipants));

  const { group } = index;
  const desc = describeNotification(index.description, contents.length !== 1);

  const onClick = useCallback(() => {
    if (props.archived) {
      return;
    }
    const func = read ? "unread" : "read";
    return api.hark[func](timebox, { group: index });
  }, [api, timebox, index, read]);

  return (
    <Col onClick={onClick} p="2">
      <Header
        archived={props.archived}
        time={time}
        read={read}
        group={group}
        contacts={props.contacts}
        authors={authors}
        description={desc}
        associations={associations}
      />
    </Col>
  );
}

