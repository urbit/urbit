import React, {useRef} from "react";
import { Box, Text, Col } from "@tlon/indigo-react";
import f from "lodash/fp";
import _ from "lodash";

import { Associations, Association, Unreads, UnreadStats } from "~/types";
import { alphabeticalOrder } from "~/logic/lib/util";
import { getUnreadCount, getNotificationCount } from "~/logic/lib/hark";
import Tile from "../components/tiles/tile";
import { useTutorialModal } from "~/views/components/useTutorialModal";
import {TUTORIAL_HOST, TUTORIAL_GROUP} from "~/logic/lib/tutorialModal";

interface GroupsProps {
  associations: Associations;
}

const sortGroupsAlph = (a: Association, b: Association) =>
  alphabeticalOrder(a.metadata.title, b.metadata.title);


const getGraphUnreads = (associations: Associations, unreads: Unreads) => (path: string) =>
  f.flow(
    f.pickBy((a: Association) => a.group === path),
    f.map('resource'),
    f.map(rid => getUnreadCount(unreads, rid, '/')),
    f.reduce(f.add, 0)
  )(associations.graph);

const getGraphNotifications = (associations: Associations, unreads: Unreads) => (path: string) =>
  f.flow(
    f.pickBy((a: Association) => a.group === path),
    f.map('resource'),
    f.map(rid => getNotificationCount(unreads, rid)),
    f.reduce(f.add, 0)
  )(associations.graph);


export default function Groups(props: GroupsProps & Parameters<typeof Box>[0]) {
  const { associations, unreads, inbox, ...boxProps } = props;

  const groups = Object.values(associations?.groups || {})
    .filter((e) => e?.group in props.groups)
    .sort(sortGroupsAlph);
  const graphUnreads = getGraphUnreads(associations || {}, unreads);
  const graphNotifications = getGraphNotifications(associations || {}, unreads);

  return (
    <>
      {groups.map((group, index) => {
        const path = group?.group;
        const unreadCount = graphUnreads(path)
        const notCount = graphNotifications(path);

        return (
          <Group
            updates={notCount}
            first={index === 0}
            unreads={unreadCount}
            path={group?.group}
            title={group.metadata.title}
            picture={group.metadata.picture}
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
  first: boolean;
}
function Group(props: GroupProps) {
  const { path, title, unreads, updates, first = false } = props;
  const anchorRef = useRef<HTMLElement>(null);
  const isTutorialGroup = path === `/ship/${TUTORIAL_HOST}/${TUTORIAL_GROUP}`;
  useTutorialModal(
    'start',
    isTutorialGroup,
    anchorRef.current
  );
  return (
    <Tile ref={anchorRef} position="relative" bg={isTutorialGroup ? 'lightBlue' : undefined} to={`/~landscape${path}`} gridColumnStart={first ? '1' : null}>
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
