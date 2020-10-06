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
const sortGroupsRecent = (recent: string[]) => (
  a: Association,
  b: Association
) => {
  //
  const aRecency = recent.findIndex((r) => a["group-path"] === r);
  const bRecency = recent.findIndex((r) => b["group-path"] === r);
  if(aRecency === -1) {
    if(bRecency === -1) {
      return 0;
    }
    return 1;
  }
  if(bRecency === -1) {
    return -1;
  }
  return Math.max(0,bRecency) - Math.max(0, aRecency);
};

const sortGroupsAlph = (a: Association, b: Association) =>
  alphabeticalOrder(a.metadata.title, b.metadata.title);

export default function Groups(props: GroupsProps & Parameters<typeof Box>[0]) {
  const { associations, ...boxProps } = props;
  const [recentGroups, setRecentGroups] = useLocalStorageState<string[]>(
    "recent-groups",
    []
  );

  const groups = Object.values(props?.associations?.contacts || {})
    .sort(sortGroupsAlph)
    .sort(sortGroupsRecent(recentGroups))

  return (
    <Box
      {...boxProps}
      ml='2'
      display="grid"
      gridAutoRows="124px"
      gridTemplateColumns="repeat(auto-fit, 124px)"
      gridGap={3}
      p={2}
    >
      {groups.map((group) => (
        <Link to={`/~landscape${group["group-path"]}`}>
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
