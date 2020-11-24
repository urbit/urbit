import React from "react";
import { Box, Text, Col } from "@tlon/indigo-react";
import f from "lodash/fp";
import _ from "lodash";

import { Associations, Association, Unreads, UnreadStats } from "~/types";
import { alphabeticalOrder } from "~/logic/lib/util";
import Tile from "../components/tiles/tile";

interface GroupsProps {
  associations: Associations;
}

const sortGroupsAlph = (a: Association, b: Association) =>
  alphabeticalOrder(a.metadata.title, b.metadata.title);


const getGraphUnreads = (associations: Associations, unreads: Unreads) => (path: string): number => 
  f.flow(
    (x) => x['graph'],
    f.pickBy((_v, key) => associations.graph?.[key]["group-path"] === path),
    f.map((x: Record<string, UnreadStats>) => 0), // x?.['/']?.unreads?.size),
    f.reduce(f.add, 0)
  )(unreads);

export default function Groups(props: GroupsProps & Parameters<typeof Box>[0]) {
  const { associations, unreads, inbox, ...boxProps } = props;

  const groups = Object.values(associations?.contacts || {})
    .filter((e) => e?.["group-path"] in props.groups)
    .sort(sortGroupsAlph);
  const graphUnreads = getGraphUnreads(associations || {}, unreads);

  return (
    <>
      {groups.map((group) => {
        const path = group?.["group-path"];
        const unreadCount = graphUnreads(path)

        return (
          <Group
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
  updates?: number;
  unreads: number;
}
function Group(props: GroupProps) {
  const { path, title, unreads } = props;
  return (
    <Tile to={`/~landscape${path}`}>
      <Col height="100%" justifyContent="space-between">
        <Text>{title}</Text>
        {unreads > 0 && 
          (<Text color="blue" gray>{unreads} update{unreads !== 1 && 's'} </Text>)
        }
      </Col>
    </Tile>
  );
}
