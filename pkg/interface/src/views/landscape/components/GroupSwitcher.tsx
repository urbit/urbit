import React from "react";
import {
  Center,
  Box,
  Col,
  Row,
  Text,
  IconButton,
  Button,
  Icon,
} from "@tlon/indigo-react";
import { uxToHex } from "~/logic/lib/util";
import { Link } from "react-router-dom";

import { Association, Associations } from "~/types/metadata-update";
import { Dropdown } from "~/views/components/Dropdown";
import { Workspace } from "~/types";
import { getTitleFromWorkspace } from "~/logic/lib/workspace";

const GroupSwitcherItem = ({ to, children, bottom = false, ...rest }) => (
  <Link to={to}>
    <Box
      py={1}
      {...rest}
      borderBottom={bottom ? 0 : 1}
      borderBottomColor="lightGray"
    >
      <Row p={2} alignItems="center">
        {children}
      </Row>
    </Box>
  </Link>
);

function RecentGroups(props: { recent: string[]; associations: Associations }) {
  const { associations, recent } = props;
  if (recent.length < 2) {
    return null;
  }

  return (
    <Col borderBottom={1} borderBottomColor="lightGray" p={1}>
      <Box fontSize={0} px={1} py={2} color="gray">
        Recent Groups
      </Box>
      {props.recent.filter((e) => {
        return (e in associations?.contacts);
      }).slice(1, 5).map((g) => {
        const assoc = associations.contacts[g];
        const color = uxToHex(assoc?.metadata?.color || "0x0");
        return (
          <Link key={g} style={{ minWidth: 0 }} to={`/~landscape${g}`}>
          <Row px={1} pb={2} alignItems="center">
            <Box
              borderRadius={1}
              border={1}
              borderColor="lightGray"
              height="16px"
              width="16px"
              bg={`#${color}`}
              mr={2}
              display="block"
              flexShrink='0'
            />
              <Text verticalAlign='top' maxWidth='100%' overflow='hidden' display='inline-block' style={{ textOverflow: 'ellipsis', whiteSpace: 'pre' }}>{assoc?.metadata?.title}</Text>
            </Row>
          </Link>
        );
      })}
    </Col>
  );
}

export function GroupSwitcher(props: {
  associations: Associations;
  workspace: Workspace;
  baseUrl: string;
  recentGroups: string[];
}) {
  const { associations, workspace, isAdmin } = props;
  const title = getTitleFromWorkspace(associations, workspace);
  const navTo = (to: string) => `${props.baseUrl}${to}`;
  return (
    <Box zIndex="2" position="sticky" top="0px" p={2}>
      <Col
        justifyContent="center"
        bg="white"
        borderRadius={1}
        border={1}
        borderColor="washedGray"
      >
        <Row alignItems="center" justifyContent="space-between">
          <Dropdown
            width="231px"
            alignY="top"
            options={
              <Col
                borderRadius={1}
                border={1}
                borderColor="lightGray"
                bg="white"
                width="100%"
                alignItems="stretch"
              >
                {(props.baseUrl === '/~landscape/home') ?
                  <GroupSwitcherItem to="">
                  <Icon
                    mr={2}
                    color="gray"
                    display="block"
                    icon="Groups"
                  />
                  <Text>All Groups</Text>
                </GroupSwitcherItem>
                :
                <GroupSwitcherItem to="/~landscape/home">
                  <Icon
                    mr={2}
                    color="gray"
                    display="block"
                    icon="Mail"
                  />
                  <Text>DMs + Drafts</Text>
                </GroupSwitcherItem>}
                <RecentGroups
                  recent={props.recentGroups}
                  associations={props.associations}
                />
                <GroupSwitcherItem to="/~landscape/new">
                  <Icon mr="2" color="gray" icon="CreateGroup" />
                  <Text> New Group</Text>
                </GroupSwitcherItem>
                <GroupSwitcherItem to="/~landscape/join">
                  <Icon mr="2" color="gray" icon="Plus" />
                  <Text> Join Group</Text>
                </GroupSwitcherItem>
                {workspace.type === "group" && (
                  <>
                    <GroupSwitcherItem to={navTo("/popover/participants")}>
                      <Icon
                        mr={2}
                        color="gray"
                        icon="Node"
                      />
                      <Text> Participants</Text>
                    </GroupSwitcherItem>
                    <GroupSwitcherItem to={navTo("/popover/settings")}>
                      <Icon
                        mr={2}
                        color="gray"
                        icon="Gear"
                      />
                      <Text> Group Settings</Text>
                    </GroupSwitcherItem>
                    {isAdmin && (<GroupSwitcherItem bottom to={navTo("/invites")}>
                      <Icon
                        mr={2}
                        color="blue"
                        icon="Users"
                      />
                      <Text color="blue">Invite to group</Text>
                    </GroupSwitcherItem>)}
                  </>
                )}
              </Col>
            }
          >
            <Row p={2} alignItems="center" width='100%' minWidth='0'>
              <Row alignItems="center" mr={1} flex='1' width='100%' minWidth='0'>
                <Text overflow='hidden' display='inline-block' flexShrink='1' style={{ textOverflow: 'ellipsis', whiteSpace: 'pre'}}>{title}</Text>
                <Icon size='12px' ml='1' mt="0px" display="inline-block" icon="ChevronSouth" />
              </Row>
            </Row>
          </Dropdown>
          <Row pr={1} justifyContent="flex-end" alignItems="center">
            {(workspace.type === "group") && (
              <>
                {isAdmin && (<Link to={navTo("/invites")}>
                  <Icon
                    display="block"
                    color='blue'
                    icon="Users"
                  />
                </Link>)}
                <Link to={navTo("/popover/settings")}>
                  <Icon color='gray' display="block" m={2} icon="Gear" />
                </Link>
              </>
            )}
          </Row>
        </Row>
      </Col>
    </Box>
  );
}
