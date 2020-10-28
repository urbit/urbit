import React, { useCallback } from "react";
import { Box, Col, Text, Row } from "@tlon/indigo-react";
import { Link, Switch, Route } from "react-router-dom";

import { Body } from "~/views/components/Body";
import { PropFunc } from "~/types/util";
import Inbox from "./inbox";
import NotificationPreferences from "./preferences";

const baseUrl = "/~notifications";

const HeaderLink = (
  props: PropFunc<typeof Text> & { view?: string; current: string }
) => {
  const { current, view, ...textProps } = props;
  const to = view ? `${baseUrl}/${view}` : baseUrl;
  const active = view ? current === view : !current;

  return (
    <Link to={to}>
      <Text px="2" {...textProps} gray={!active} />
    </Link>
  );
};

export default function NotificationsScreen(props: any) {
  const relativePath = (p: string) => baseUrl + p;
  return (
    <Switch>
      <Route
        path={[relativePath("/:view"), relativePath("")]}
        render={(routeProps) => {
          const { view } = routeProps.match.params;
          return (
            <Body>
              <Col height="100%">
                <Row
                  p="3"
                  alignItems="center"
                  height="48px"
                  justifyContent="space-between"
                  width="100%"
                  borderBottom="1"
                  borderBottomColor="washedGray"
                >
                  <Box>Updates </Box>
                  <Row>
                    <Box>
                      <HeaderLink current={view} view="">
                        Inbox
                      </HeaderLink>
                    </Box>
                    <Box>
                      <HeaderLink current={view} view="archive">
                        Archive
                      </HeaderLink>
                    </Box>
                    <Box>
                      <HeaderLink current={view} view="preferences">
                        Preferences
                      </HeaderLink>
                    </Box>
                  </Row>
                  <Box>
                    <Text mr="1" gray>
                      Filter:
                    </Text>
                    All
                  </Box>
                </Row>
                {view === "archive" && (
                  <Inbox
                    {...props}
                    archive={props.archivedNotifications}
                    showArchive
                  />
                )}
                {view === "preferences" && (
                  <NotificationPreferences
                    graphConfig={props.notificationsGraphConfig}
                    api={props.api}
                    dnd={props.doNotDisturb}
                  />
                )}
                {!view && <Inbox {...props} />}
              </Col>
            </Body>
          );
        }}
      />
    </Switch>
  );
}
