import React from 'react';
import { Box, Col, Row, Text, IconButton, Button, Icon } from '@tlon/indigo-react';
import { Link } from 'react-router-dom';

import { Association, } from "~/types/metadata-update";
import { Dropdown } from "~/views/components/Dropdown";


const GroupSwitcherItem = ({ to, children, ...rest }) => (
  <Link to={to}>
    <Row {...rest} px={1} mb={2} alignItems="center">
      {children}
    </Row>
  </Link>
);

export function GroupSwitcher(props: { association: Association; baseUrl: string }) {
  const { title } = props.association.metadata;
  const navTo = (to: string) => `${props.baseUrl}${to}`;
  return (
    <Box position="sticky" top="0px" p={2}>
      <Col bg="white" borderRadius={1} border={1} borderColor="washedGray">
        <Row justifyContent="space-between">
          <Dropdown
            width="220px"
            options={
              <Col width="100%" alignItems="flex-start">
                <Row
                  alignItems="center"
                  px={2}
                  pb={2}
                  my={2}
                  borderBottom={1}
                  borderBottomColor="washedGray"
                >
                  <img
                    src="/~landscape/img/groups.png"
                    height="12px"
                    width="12px"
                  />
                  <Text ml={2}>Switch Groups</Text>
                </Row>
                <GroupSwitcherItem to={navTo("/popover/participants")}>
                  <Icon mr={2} icon="Circle" />
                  <Text> Participants</Text>
                </GroupSwitcherItem>
                <GroupSwitcherItem to={navTo("/popover/settings")}>
                  <Icon mr={2} icon="Circle" />
                  <Text> Settings</Text>
                </GroupSwitcherItem>
                <GroupSwitcherItem to={navTo("/popover/settings")}>
                  <Icon mr={2} icon="Circle" />
                  <Text>Group Settings</Text>
                </GroupSwitcherItem>
                <GroupSwitcherItem to={navTo("/invites")}>
                  <Icon mr={2} fill="blue" icon="Circle" />
                  <Text color="blue">Invite to group</Text>
                </GroupSwitcherItem>
              </Col>
            }
          >
            <Button width="max-content">
              <Box display="flex">
                <Box width="max-content">
                  <Text>{title}</Text>
                </Box>
                <Icon icon="ChevronSouth" />
              </Box>
            </Button>
          </Dropdown>
          <Link to={navTo("/popover/settings")}>
            <IconButton icon="MagnifyingGlass" />
          </Link>
        </Row>
      </Col>
    </Box>
  );
}
