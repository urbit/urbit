import React, { useCallback, useState } from "react";
import _ from 'lodash';
import { Box, Col, Text, Row } from "@tlon/indigo-react";
import { Link, Switch, Route } from "react-router-dom";

import { Body } from "~/views/components/Body";
import { PropFunc } from "~/types/util";
import Inbox from "./inbox";
import NotificationPreferences from "./preferences";
import { Dropdown } from "~/views/components/Dropdown";
import { Formik } from "formik";
import { FormikOnBlur } from "~/views/components/FormikOnBlur";
import GroupSearch from "~/views/components/GroupSearch";

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

interface NotificationFilter {
  groups: string[];
}

export default function NotificationsScreen(props: any) {
  const relativePath = (p: string) => baseUrl + p;

  const [filter, setFilter] = useState<NotificationFilter>({ groups: [] });
  const onSubmit = async (values: { groups: string }) => {
    setFilter({ groups: values.groups ? [values.groups] : [] });
  };
  const groupFilterDesc =
    filter.groups.length === 0
      ? "All"
      : filter.groups
          .map((g) => props.associations?.contacts?.[g]?.metadata?.title)
          .join(", ");
  return (
    <Switch>
      <Route
        path={[relativePath("/:view"), relativePath("")]}
        render={(routeProps) => {
          const { view } = routeProps.match.params;
          return (
            <Body>
              <Col overflowY="hidden" height="100%">
                <Row
                  p="3"
                  alignItems="center"
                  height="48px"
                  justifyContent="space-between"
                  width="100%"
                  borderBottom="1"
                  borderBottomColor="washedGray"
                >
                  <Text>Updates</Text>
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
                  <Dropdown
                    alignX="right"
                    alignY="top"
                    options={
                      <Col
                        p="2"
                        backgroundColor="white"
                        border={1}
                        borderRadius={1}
                        borderColor="lightGray"
                        gapY="2"
                      >
                        <FormikOnBlur
                          initialValues={filter}
                          onSubmit={onSubmit}
                        >
                          <GroupSearch
                            id="groups"
                            label="Filter Groups"
                            caption="Only show notifications from this group"
                            associations={props.associations}
                          />
                        </FormikOnBlur>
                      </Col>
                    }
                  >
                    <Box>
                      <Text mr="1" gray>
                        Filter:
                      </Text>
                      <Text>{groupFilterDesc}</Text>
                    </Box>
                  </Dropdown>
                </Row>
                {view === "archive" && (
                  <Inbox
                    {...props}
                    archive={props.archivedNotifications}
                    showArchive
                    filter={filter.groups}
                  />
                )}
                {view === "preferences" && (
                  <NotificationPreferences
                    graphConfig={props.notificationsGraphConfig}
                    api={props.api}
                    dnd={props.doNotDisturb}
                  />
                )}
                {!view && <Inbox {...props} filter={filter.groups} />}
              </Col>
            </Body>
          );
        }}
      />
    </Switch>
  );
}
