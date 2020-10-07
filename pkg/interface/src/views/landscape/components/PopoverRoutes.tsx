import React, { useRef, useCallback } from "react";
import { Route, Switch, RouteComponentProps, Link } from "react-router-dom";
import { Box, Row, Col, Icon, Text } from "@tlon/indigo-react";
import { useOutsideClick } from "~/logic/lib/useOutsideClick";
import { HoverBoxLink } from "~/views/components/HoverBox";
import { Contacts } from "~/types/contact-update";
import { Group } from "~/types/group-update";
import { Association } from "~/types/metadata-update";
import GlobalApi from "~/logic/api/global";
import {S3State} from "~/types";

import { ContactCard } from "./ContactCard";
import { GroupSettings } from "./GroupSettings";
import { Participants } from "./Participants";


const SidebarItem = ({ selected, icon, text, to }) => {
  return (
    <HoverBoxLink
      to={to}
      selected={selected}
      bg="white"
      bgActive="washedGray"
      display="flex"
      px={3}
      py={1}
    >
      <Icon icon={icon} mr='2'/>
      <Text color={selected ? "black" : "gray"}>{text}</Text>
    </HoverBoxLink>
  );
};

export function PopoverRoutes(
  props: {
    baseUrl: string;
    contacts: Contacts;
    group: Group;
    association: Association;
    s3: S3State;
    api: GlobalApi;
  } & RouteComponentProps
) {
  const relativeUrl = (url: string) => `${props.baseUrl}/popover${url}`;
  const innerRef = useRef(null);

  const onOutsideClick = useCallback(() => {
    props.history.push(props.baseUrl);
  }, [props.history.push, props.baseUrl]);
  useOutsideClick(innerRef, onOutsideClick);

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
                  gridTemplateColumns={["100%", "150px 1fr"]}
                  height="100%"
                  width="100%"
                >
                  <Col
                    display={!!view ? ["none", "flex"] : "flex"}
                    py={3}
                    borderRight={1}
                    borderRightColor="washedGray"
                  >
                    <SidebarItem
                      icon="Node"
                      selected={view === "participants"}
                      to={relativeUrl("/participants")}
                      text="Participants"
                    />
                    <SidebarItem
                      icon="Gear"
                      selected={view === "settings"}
                      to={relativeUrl("/settings")}
                      text="Group Settings"
                    />
                    <SidebarItem
                      icon="Smiley"
                      selected={view === "profile"}
                      to={relativeUrl("/profile")}
                      text="Group Profile"
                    />
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
                        group={props.group}
                        association={props.association}
                        api={props.api}
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
                    {view === "profile" && (
                      <ContactCard
                        contact={props.contacts[window.ship]}
                        api={props.api}
                        path={props.association["group-path"]}
                        s3={props.s3}
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
