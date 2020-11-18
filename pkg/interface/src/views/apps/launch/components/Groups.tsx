import React from "react";
import { Box, Text } from "@tlon/indigo-react";

import { Associations, Association } from "~/types";
import { alphabeticalOrder } from "~/logic/lib/util";
import Tile from '../components/tiles/tile';

interface GroupsProps {
  associations: Associations;
}

const sortGroupsAlph = (a: Association, b: Association) =>
  alphabeticalOrder(a.metadata.title, b.metadata.title);

export default function Groups(props: GroupsProps & Parameters<typeof Box>[0]) {
  const { associations, ...boxProps } = props;

  const groups = Object.values(associations?.contacts || {})
    .filter(e => e['group-path'] in props.groups)
    .sort(sortGroupsAlph);

  return (
    <>
      {groups.map((group) => (
        <Tile to={`/~landscape${group["group-path"]}`}>
          <Text>{group.metadata.title}</Text>
        </Tile>
      ))}
    </>
  );
}
