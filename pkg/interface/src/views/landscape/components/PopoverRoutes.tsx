import React, { useRef, useCallback } from "react";
import { Route, Switch, RouteComponentProps, Link } from "react-router-dom";
import { Box, Row, Col, Icon, Text } from "@tlon/indigo-react";
import { useOutsideClick } from "~/logic/lib/useOutsideClick";
import { HoverBoxLink } from "~/views/components/HoverBox";
import { Contacts, Contact } from "~/types/contact-update";
import { Group } from "~/types/group-update";
import { Association } from "~/types/metadata-update";
import GlobalApi from "~/logic/api/global";
import { GroupNotificationsConfig, S3State, Associations } from "~/types";

import { GroupSettings } from "./GroupSettings/GroupSettings";
import { Participants } from "./Participants";
import {useHashLink} from "~/logic/lib/useHashLink";
import {DeleteGroup} from "./DeleteGroup";
import {resourceFromPath} from "~/logic/lib/group";

const SidebarItem = ({ selected, icon, text, to, children = null }) => {
  return (
    <HoverBoxLink
      to={to}
      selected={selected}
      bg="white"
      bgActive="washedGray"
      display="flex"
      px="3"
      py="1"
      justifyContent="space-between"
    >
      <Row>
        <Icon icon={icon} mr='2'/>
        <Text color={selected ? "black" : "gray"}>{text}</Text>
      </Row>
      {children}
    </HoverBoxLink>
  );
};

export function PopoverRoutes(
  props: {
    baseUrl: string;
    contacts: Contacts;
    group: Group;
    association: Association;
    associations: Associations;
    s3: S3State;
    api: GlobalApi;
    notificationsGroupConfig: GroupNotificationsConfig;
    rootIdentity: Contact;
  } & RouteComponentProps
) {
  const relativeUrl = (url: string) => `${props.baseUrl}/popover${url}`;
  const innerRef = useRef(null);

  const onOutsideClick = useCallback(() => {
    props.history.push(props.baseUrl);
  }, [props.history.push, props.baseUrl]);
  useOutsideClick(innerRef, onOutsideClick);

  useHashLink();

  const groupSize = props.group.members.size;

  const owner = resourceFromPath(props.association.group).ship.slice(1) === window.ship;

  const admin = props.group?.tags?.role?.admin.has(window.ship) || false;

  return (
    <Switch>
      <Route
        path={[relativeUrl("/:view"), relativeUrl("")]}
        render={(routeProps) => {
          const { view } = routeProps.match.params;
          return (
            <Box
              px={[3, 5, 8]}
              py={[3, 5]}
              backgroundColor='scales.black30'
              left="0px"
              top="0px"
              width="100%"
              height="100%"
              zIndex={4}
              position="fixed"
            >
              <Box
                ref={innerRef}
                border={1}
                borderColor="washedGray"
                borderRadius={1}
                width="100%"
                height="100%"
                bg="white"
              >
                <Box
                  display="grid"
                  gridTemplateRows={["32px 1fr", "100%"]}
                  gridTemplateColumns={["100%", "250px 1fr"]}
                  height="100%"
                  width="100%"
                >
                  <Col
                    display={!!view ? ["none", "flex"] : "flex"}
                    borderRight={1}
                    borderRightColor="washedGray"
                  >
                    <Text my="4" mx="3" fontWeight="600" fontSize="2">Group Settings</Text>
                    <Col gapY="2">
                      <Text my="1" mx="3" gray>Group</Text>
                      <SidebarItem
                        icon="Inbox"
                        to={relativeUrl("/settings#notifications")}
                        text="Notifications"
                      />
                      <SidebarItem
                        icon="Users"
                        to={relativeUrl("/participants")}
                        text="Participants"
                        selected={view === "participants"}
                      ><Text gray>{groupSize}</Text>
                      </SidebarItem>
                      { admin && (
                        <>
                          <Box pt="3" mb="1" mx="3">
                            <Text gray>Administration</Text>
                          </Box>
                          <SidebarItem
                            icon="Groups"
                            to={relativeUrl("/settings#group-details")}
                            text="Group Details"
                          />
                          <SidebarItem
                            icon="Spaces"
                            to={relativeUrl("/settings#channels")}
                            text="Channel Management"
                          />
                        </>
                      )}
                      <DeleteGroup owner={owner} api={props.api} association={props.association} />
                    </Col>
                  </Col>
                  <Box
                    gridArea={"1 / 1 / 2 / 2"}
                    p={2}
                    display={["auto", "none"]}
                  >
                    <Link to={!!view ? relativeUrl("") : props.baseUrl}>
                      <Text>{"<- Back"}</Text>
                    </Link>
                  </Box>
                  <Box overflow="hidden">
                    {view === "settings" && (
                      <GroupSettings
                        baseUrl={`${props.baseUrl}/popover`}
                        group={props.group}
                        association={props.association}
                        api={props.api}
                        notificationsGroupConfig={props.notificationsGroupConfig}
                        associations={props.associations}
                        s3={props.s3}
                      />
                    )}
                    {view === "participants" && (
                      <Participants
                        group={props.group}
                        contacts={props.contacts}
                        association={props.association}
                        api={props.api}
                      />
                    )}
                  </Box>
                </Box>
              </Box>
            </Box>
          );
        }}
      />
    </Switch>
  );
}
