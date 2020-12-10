import React from "react";
import { Box, Text, Col } from "@tlon/indigo-react";
import f from "lodash/fp";
import _ from "lodash";

import { Associations, Association, Unreads } from "~/types";
import { alphabeticalOrder } from "~/logic/lib/util";
import Tile from "../components/tiles/tile";

interface GroupsProps {
  associations: Associations;
}

const sortGroupsAlph = (a: Association, b: Association) =>
  alphabeticalOrder(a.metadata.title, b.metadata.title);

const getKindUnreads = (associations: Associations) => (path: string) => (
  kind: "chat" | "graph"
): ((unreads: Unreads) => number) =>
  f.flow(
    (x) => x[kind],
    f.pickBy((_v, key) => associations[kind]?.[key]?.["group-path"] === path),
    f.values,
    f.reduce(f.add, 0)
  );

export default function Groups(props: GroupsProps & Parameters<typeof Box>[0]) {
  const { associations, unreads, ...boxProps } = props;

  const groups = Object.values(associations?.contacts || {})
    .filter((e) => e?.["group-path"] in props.groups)
    .sort(sortGroupsAlph);
  const getUnreads = getKindUnreads(associations || {});

  return (
    <>
      {groups.map((group, index) => {
        const path = group?.["group-path"];
        const unreadCount = (["chat", "graph"] as const)
          .map(getUnreads(path))
          .map((f) => f(unreads))
          .reduce(f.add, 0);
        return (
          <Group
            first={index === 0}
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
  unreads: number;
  first: boolean;
}
function Group(props: GroupProps) {
  const { path, title, unreads, first = false } = props;
  return (
    <Tile to={`/~landscape${path}`} gridColumnStart={first ? '1' : null}>
      <Col height="100%" justifyContent="space-between">
        <Text>{title}</Text>
        {unreads > 0 && 
          (<Text color="blue" gray>{unreads} update{unreads !== 1 && 's'} </Text>)
        }
      </Col>
    </Tile>
  );
}
