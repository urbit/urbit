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
import {Workspace} from "~/types";
import {getTitleFromWorkspace} from "~/logic/lib/workspace";

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
      <Box px={1} py={2} color="gray">
        Recent Groups
      </Box>
      {props.recent.slice(1, 5).map((g) => {
        const assoc = associations.contacts[g];
        const color = uxToHex(assoc?.metadata?.color);
        return (
          <Row key={g} px={1} pb={2} alignItems="center">
            <Box
              borderRadius={1}
              border={1}
              borderColor="lightGray"
              height="16px"
              width="16px"
              bg={color}
              mr={2}
              display="block"
            />
            <Link to={`/~groups${g}`}>{assoc?.metadata?.title}</Link>
          </Row>
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
  const { associations, workspace } = props;
  const title = getTitleFromWorkspace(associations, workspace)
  const navTo = (to: string) => `${props.baseUrl}${to}`;
  return (
    <Box position="sticky" top="0px" p={2}>
      <Col
        justifyContent="center"
        bg="white"
        borderRadius={1}
        border={1}
        borderColor="washedGray"
      >
        <Row alignItems="center" justifyContent="space-between">
          <Dropdown
            width="220px"
            alignY="top"
            options={
              <Col bg="white" width="100%" alignItems="stretch">
                <GroupSwitcherItem to="">
                  <Icon
                    mr={2}
                    stroke="gray"
                    fill="rgba(0,0,0,0)"
                    display="block"
                    icon="Groups"
                  />
                  <Text>All Groups</Text>
                </GroupSwitcherItem>
                <RecentGroups
                  recent={props.recentGroups}
                  associations={props.associations}
                />
                {workspace.type === 'group' && (
                  <>
                    <GroupSwitcherItem to={navTo("/popover/participants")}>
                      <Icon mr={2} fill="none" stroke="gray" icon="CircleDot" />
                      <Text> Participants</Text>
                    </GroupSwitcherItem>
                    <GroupSwitcherItem to={navTo("/popover/settings")}>
                      <Icon mr={2} fill="none" stroke="gray" icon="Gear" />
                      <Text> Settings</Text>
                    </GroupSwitcherItem>
                    <GroupSwitcherItem bottom to={navTo("/invites")}>
                      <Icon
                        mr={2}
                        fill="rgba(0,0,0,0)"
                        stroke="blue"
                        icon="CreateGroup"
                      />
                      <Text color="blue">Invite to group</Text>
                    </GroupSwitcherItem>
                  </>
                )}
              </Col>
            }
          >
            <Box p={2} alignItems="center" display="flex">
              <Box mr={1}>
                <Text>{title}</Text>
              </Box>
              <Icon mt="2px" display="block" icon="ChevronSouth" />
            </Box>
          </Dropdown>
          <Row collapse pr={1} justifyContent="flex-end" alignItems="center">
            { workspace.type === 'group' && (
              <>
                <Link to={navTo("/invites")}>
                  <Icon
                    display="block"
                    fill="rgba(0,0,0,0)"
                    stroke="blue"
                    icon="CreateGroup"
                  />
                </Link>
                <Link to={navTo("/popover/settings")}>
                  <Icon display="block" ml={2} icon="Gear" />
                </Link>
              </>
            )}
          </Row>
        </Row>
      </Col>
    </Box>
  );
}
