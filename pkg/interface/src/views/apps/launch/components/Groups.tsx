import React from "react";
import { Box, Text } from "@tlon/indigo-react";
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
  return Math.max(0, aRecency) - Math.max(0,bRecency);
};

const sortGroupsAlph = (a: Association, b: Association) =>
  alphabeticalOrder(a.metadata.title, b.metadata.title);

export default function Groups(props: GroupsProps & Parameters<typeof Box>[0]) {
  const { associations, invites, api, ...boxProps } = props;
  const [recentGroups, setRecentGroups] = useLocalStorageState<string[]>(
    "recent-groups",
    []
  );

  const incomingGroups = Object.values(invites?.['/contacts'] || {});
  const getKeyByValue = (object, value) => {
    return Object.keys(object).find(key => object[key] === value);
  }

  const groups = Object.values(associations?.contacts || {})
    .sort(sortGroupsAlph)
    .sort(sortGroupsRecent(recentGroups))

  const acceptInvite = (invite) => {
    const [, , ship, name] = invite.path.split('/');
    const resource = { ship, name };
    return api.contacts.join(resource).then(() => {
      api.invite.accept('/contacts', getKeyByValue(invites['/contacts'], invite));
    });
  };

  return (
    <Box
      {...boxProps}
      ml='2'
      display="grid"
      gridAutoRows="124px"
      gridTemplateColumns="repeat(auto-fit, 124px)"
      gridGap={3}
      px={2}
      pt={2}
      pb="7"
    >
      {incomingGroups.map((invite) => (
        <Box
          height='100%'
          width='100%'
          bg='white'
          border='1'
          borderRadius='2'
          borderColor='lightGray'
          p='2'
          fontSize='0'
        >
          <Text display='block' pb='2' gray>You have been invited to:</Text>
          <Text display='inline-block' overflow='hidden' maxWidth='100%' style={{ textOverflow: 'ellipsis', whiteSpace: 'pre' }} title={invite.path.slice(6)}>{invite.path.slice(6)}</Text>
          <Box pt='5'>
            <Text
              onClick={() => acceptInvite(invite)}
            color='blue'
            mr='2'
            cursor='pointer'>
              Accept
            </Text>
            <Text
              color='red'
              onClick={() => api.invite.decline('/contacts', getKeyByValue(invites['/contacts'], invite))}
              cursor='pointer'>
                Reject
              </Text>
          </Box>
        </Box>
      ))}
      {groups.map((group) => (
        <Link to={`/~landscape${group["group-path"]}`}>
          <Box
            height="100%"
            width="100%"
            bg="white"
            border={1}
            borderRadius={2}
            borderColor="lightGray"
            p={2}
            fontSize={0}
          >
            <Text>{group.metadata.title}</Text>
          </Box>
        </Link>
      ))}
    </Box>
  );
}
