import React from "react";
import { Box, Text, Col } from "@tlon/indigo-react";
import f from "lodash/fp";
import _ from "lodash";

import { Associations, Association, Unreads, UnreadStats } from "~/types";
import { alphabeticalOrder } from "~/logic/lib/util";
import { getUnreadCount, getNotificationCount } from "~/logic/lib/hark";
import Tile from "../components/tiles/tile";

interface GroupsProps {
  associations: Associations;
}

const sortGroupsAlph = (a: Association, b: Association) =>
  alphabeticalOrder(a.metadata.title, b.metadata.title);


const getGraphUnreads = (associations: Associations, unreads: Unreads) => (path: string) => 
  f.flow(
    f.pickBy((a: Association) => a['group-path'] === path),
    f.map('app-path'),
    f.map(appPath => getUnreadCount(unreads, appPath, '/')),
    f.reduce(f.add, 0)
  )(associations.graph);

const getGraphNotifications = (associations: Associations, unreads: Unreads) => (path: string) => 
  f.flow(
    f.pickBy((a: Association) => a['group-path'] === path),
    f.map('app-path'),
    f.map(appPath => getNotificationCount(unreads, appPath)),
    f.reduce(f.add, 0)
  )(associations.graph);


export default function Groups(props: GroupsProps & Parameters<typeof Box>[0]) {
  const { associations, unreads, inbox, ...boxProps } = props;

  const groups = Object.values(associations?.contacts || {})
    .filter((e) => e?.["group-path"] in props.groups)
    .sort(sortGroupsAlph);
  const graphUnreads = getGraphUnreads(associations || {}, unreads);
  const graphNotifications = getGraphNotifications(associations || {}, unreads);

  return (
    <>
      {groups.map((group) => {
        const path = group?.["group-path"];
        const unreadCount = graphUnreads(path)
        const notCount = graphNotifications(path);

        return (
          <Group
            updates={notCount}
            unreads={unreadCount}
            path={group?.["group-path"]}
            title={group.metadata.title}
          />
        );
      })}
    </>
  );
}

interface GroupProps {
  path: string;
  title: string;
  updates: number;
  unreads: number;
}
function Group(props: GroupProps) {
  const { path, title, unreads, updates } = props;
  return (
    <Tile to={`/~landscape${path}`}>
      <Col height="100%" justifyContent="space-between">
        <Text>{title}</Text>
        <Col>
          {unreads > 0 && 
            (<Text gray>{unreads} unread{unreads !== 1 && 's'} </Text>)
          }
          {updates > 0 && 
            (<Text mt="1" color="blue">{updates} update{updates !== 1 && 's'} </Text>)
          }
        </Col>

      </Col>
    </Tile>
  );
}
