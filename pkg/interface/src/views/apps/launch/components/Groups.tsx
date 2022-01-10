import { Box, Col, Text } from "@tlon/indigo-react";
import {
  Association,
  Associations,
  resourceAsPath,
  resourceFromPath,
  Unreads,
} from "@urbit/api";
import f from "lodash/fp";
import _ from "lodash";
import React from "react";
import { useHistory } from "react-router-dom";
import { getNotificationCount } from "~/logic/lib/hark";
import { alphabeticalOrder } from "~/logic/lib/util";
import useGroupState from "~/logic/state/group";
import useHarkState, { selHarkGraph } from "~/logic/state/hark";
import useInviteState from "~/logic/state/invite";
import useMetadataState, { usePreview } from "~/logic/state/metadata";
import useSettingsState, {
  selectCalmState,
  SettingsState,
} from "~/logic/state/settings";
import Tile from "../components/tiles/tile";
import { useQuery } from "~/logic/lib/useQuery";
import { createJoinParams } from "~/views/landscape/components/Join/Join";

export const sortGroupsAlph = (a: Association, b: Association) =>
  alphabeticalOrder(a.metadata.title, b.metadata.title);

export const getGraphUnreads = (associations: Associations) => {
  const state = useHarkState.getState();
  const selUnread = (graph: string) => {
    const { count, each } = selHarkGraph(graph)(state);
    const result = count + each.length;
    return result;
  };
  return (path: string) =>
    f.flow(
      f.pickBy((a: Association) => a.group === path),
      f.map("resource"),
      f.map(selUnread),
      f.reduce(f.add, 0)
    )(associations.graph);
};

export const getGraphNotifications = (
  associations: Associations,
  unreads: Unreads
) => (path: string) =>
  f.flow(
    f.pickBy((a: Association) => a.group === path),
    f.map("resource"),
    f.map((rid) => getNotificationCount(unreads, rid)),
    f.reduce(f.add, 0)
  )(associations.graph);

export default function Groups(props: Parameters<typeof Box>[0]) {
  const unreads = useHarkState((state) => state.unreads);
  const groupState = useGroupState((state) => state.groups);
  const associations = useMetadataState((state) => state.associations);

  const groups = Object.values(associations?.groups || {})
    .filter((e) => e?.group in groupState)
    .sort(sortGroupsAlph);
  const graphUnreads = getGraphUnreads(associations || ({} as Associations));
  const graphNotifications = getGraphNotifications(
    associations || ({} as Associations),
    unreads
  );

  const joining = useGroupState((s) =>
    _.omit(
      _.pickBy(s.pendingJoin || {}, req => req.app === 'groups' && req.progress != 'abort'),
      groups.map((g) => g.group)
    )
  );
  const invites = useInviteState(
    (s) =>
      Object.values(s.invites?.["groups"] || {}).map((inv) =>
        resourceAsPath(inv.resource)
      ) || []
  );
  const pending = _.union(invites, Object.keys(joining)).filter(group => {
    return !(group in (groupState?.groups || {})) && !(group in (associations.groups || {}))
  });

  return (
    <>
      {groups.map((group, index) => {
        const path = group?.group;
        const unreadCount = graphUnreads(path);
        const notCount = graphNotifications(path);
        return (
          <Group
            key={group?.group}
            updates={notCount}
            first={index === 0}
            unreads={unreadCount}
            path={group?.group}
            title={group.metadata.title}
          />
        );
      })}
      {pending.map((group, idx) => (
        <PendingGroup
          key={group}
          path={group}
          first={idx === 0 && groups.length === 0}
        />
      ))}
    </>
  );
}

interface PendingGroupProps {
  path: string;
  first?: boolean;
}

function PendingGroup(props: PendingGroupProps) {
  const { path, first } = props;
  const history = useHistory();
  const { preview, error } = usePreview(path);
  const title = preview?.metadata?.title || path;
  const { toQuery } = useQuery();
  const onClick = () => {
    history.push(toQuery(createJoinParams('groups', path, null, false)));
  };

  const joining = useGroupState((s) => s.pendingJoin[path]?.progress);

  return (
    <Tile gridColumnStart={first ? 1 : undefined}>
      <Col
        onClick={onClick}
        width="100%"
        height="100%"
        justifyContent="space-between"
      >
        <Box>
          <Text gray>{title}</Text>
        </Box>
        <Box>
          {!joining ? (
            <Text color="blue">Invited</Text>
          ) : joining === 'no-perms' || joining == 'strange' ? (
            <Text color="red">Join Failed</Text>
          ) : joining !== "done" ? (
            <Text gray>Joining...</Text>
          ) : (
            <Text color="blue">Recently joined</Text>
          )}
        </Box>
      </Col>
    </Tile>
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
  const { hideUnreads } = useSettingsState(selectCalmState);
  const request = useGroupState((s) => s.pendingJoin[path]);
  return (
    <Tile
      position="relative"
      to={`/~landscape${path}`}
      gridColumnStart={first ? 1 : null}
    >
      <Col height="100%" justifyContent="space-between">
        <Text>{title}</Text>
        {!hideUnreads && (
          <Col>
            {!!request ? <Text color="blue">New group</Text> : null}
            {updates > 0 && (
              <Text mt={1} color="blue">
                {updates} update{updates !== 1 && "s"}{" "}
              </Text>
            )}
            {unreads > 0 && <Text color="lightGray">{unreads}</Text>}
          </Col>
        )}
      </Col>
    </Tile>
  );
}
