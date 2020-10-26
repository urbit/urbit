import React from "react";
import { Route, Link, Switch } from "react-router-dom";
import Helmet from 'react-helmet';

import { Box, Text, Row, Col, Icon } from "@tlon/indigo-react";

import { Sigil } from "~/logic/lib/sigil";
import { uxToHex, MOBILE_BROWSER_REGEX } from "~/logic/lib/util";

import Settings from "./components/settings";
import { ContactCard } from "~/views/landscape/components/ContactCard";

const SidebarItem = ({ children, view, current }) => {
  const selected = current === view;
  const color = selected ? "blue" : "black";
  return (
    <Link to={`/~profile/${view}`}>
      <Row
        alignItems="center"
        verticalAlign="middle"
        py={1}
        px={3}
        backgroundColor={selected ? "washedBlue" : "white"}
      >
        <Icon mr={2} display="inline-block" icon="Circle" color={color} />
        <Text color={color} fontSize={0}>
          {children}
        </Text>
      </Row>
    </Link>
  );
};

export default function ProfileScreen(props: any) {
  const { ship, dark } = props;
  return (
    <>
    <Helmet defer={false}>
      <title>OS1 - Profile</title>
    </Helmet>
    <Switch>
    <Route
      path={["/~profile/:view", "/~profile"]}
      render={({ match, history }) => {
        const { view } = match.params;
        const contact = props.contacts?.["/~/default"]?.[window.ship];
        const sigilColor = contact?.color
          ? `#${uxToHex(contact.color)}`
          : dark
          ? "#FFFFFF"
          : "#000000";
        if(!contact) {
          return null;
        }
        if (!view && !MOBILE_BROWSER_REGEX.test(window.navigator.userAgent)) {
          history.replace("/~profile/identity");
        }

        return (
          <Box height="100%" px={[0, 3]} pb={[0, 3]} borderRadius={1}>
            <Box
              height="100%"
              width="100%"
              display="grid"
              gridTemplateColumns={["100%", "200px 1fr"]}
              gridTemplateRows={["48px 1fr", "1fr"]}
              borderRadius={1}
              bg="white"
              border={1}
              borderColor="washedGray"
            >
              <Col
                display={!view ? "flex" : ["none", "flex"]}
                alignItems="center"
                borderRight={1}
                borderColor="washedGray"
              >
                <Box width="100%" borderBottom={1} borderBottomColor="washedGray">
                  <Box
                    mx="auto"
                    bg={sigilColor}
                    borderRadius={8}
                    my={4}
                    height={128}
                    width={128}
                    display="flex"
                    justifyContent="center"
                    alignItems="center"
                  >
                    <Sigil ship={`~${ship}`} size={80} color={sigilColor} />
                  </Box>
                </Box>
                <Box width="100%" py={3} zIndex='2'>
                  <SidebarItem current={view} view="identity">
                    Your Identity
                  </SidebarItem>
                  <SidebarItem current={view} view="settings">
                    Ship Settings
                  </SidebarItem>
                </Box>
              </Col>
              <Box
                display={!view ? "none" : ["flex", "none"]}
                alignItems="center"
                px={3}
                borderBottom={1}
                borderBottomColor="washedGray"
              >
                <Link to="/~profile">{"<- Back"}</Link>
              </Box>
              <Box overflowY="auto" flexGrow={1}>
                {view === "settings" && <Settings {...props} />}

                {view === "identity" && (
                  <ContactCard
                    contact={contact}
                    path="/~/default"
                    api={props.api}
                    s3={props.s3}
                  />
                )}
              </Box>
            </Box>
          </Box>
        );
      }}
    ></Route>
      </Switch>
    </>
  );
}
