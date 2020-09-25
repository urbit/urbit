import React from "react";
import { Box } from "@tlon/indigo-react";
import { Link } from "react-router-dom";

import { useLocalStorageState } from "~/logic/lib/useLocalStorageState";
import { Associations, Association } from "~/types";
import { alphabeticalOrder } from "~/logic/lib/util";

interface GroupsProps {
  associations: Associations;
}

// Sort by recent, then by channel size? Should probably sort
// by num unreads when notif-store drops
const sortGroups = (_assocs: Associations, recent: string[]) => (
  a: Association,
  b: Association
) => {
  return alphabeticalOrder(a.metadata.title, b.metadata.title);
  //
  const aRecency = recent.findIndex((r) => a["group-path"] === r);
  const bRecency = recent.findIndex((r) => b["group-path"] === r);
  const diff =
    ((aRecency !== -1 || bRecency !== -1) && bRecency - aRecency) || 0;
  if (diff !== 0) {
    return diff;
  }
};

export default function Groups(props: GroupsProps & Parameters<typeof Box>[0]) {
  const { associations, ...boxProps } = props;
  const [recentGroups, setRecentGroups] = useLocalStorageState<string[]>(
    "recent-groups",
    []
  );

  const groups = Object.values(props?.associations?.contacts || {}).sort(
    sortGroups(props.associations, recentGroups)
  );

  return (
    <Box
      display="grid"
      gridAutoRows="124px"
      gridAutoColumns="124px"
      gridGap={3}
      p={2}
      width="100%"
      gridAutoFlow="column"
      {...boxProps}
    >
      {groups.map((group) => (
        <Link to={`/~groups${group["group-path"]}`}>
          <Box
            height="100%"
            width="100%"
            bg="white"
            border={1}
            borderRadius={1}
            borderColor="lightGray"
            p={2}
            fontSize={0}
          >
            {group.metadata.title}
          </Box>
        </Link>
      ))}
    </Box>
  );
}
