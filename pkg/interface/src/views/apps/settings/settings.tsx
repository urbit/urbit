import React, { ReactNode } from "react";
import { Route, Link, Switch } from "react-router-dom";
import Helmet from "react-helmet";

import { Box, Text, Row, Col, Icon, BaseImage } from "@tlon/indigo-react";

import Settings from "./components/settings";
import { NotificationPreferences } from "./components/lib/NotificationPref";
import DisplayForm from './components/lib/DisplayForm';
import S3Form from "./components/lib/S3Form";
import useLocalState from "~/logic/state/local";
import {CalmPrefs} from "./components/lib/CalmPref";
import SecuritySettings from "./components/lib/Security";
import {LeapSettings} from "./components/lib/LeapSettings";

export const Skeleton = (props: { children: ReactNode }) => (
  <Box height="100%" width="100%" px={[0, 3]} pb={[0, 3]} borderRadius={1}>
    <Box
      height="100%"
      width="100%"
      borderRadius={1}
      bg="white"
      border={1}
      borderColor="washedGray"
      overflowY="auto"
    >
      {props.children}
    </Box>
  </Box>
);

export default function SettingsScreen(props: any) {
  const { ship, dark } = props;
  const hideAvatars = useLocalState((state) => state.hideAvatars);
  return (
    <>
      <Helmet defer={false}>
        <title>Landscape - Settings</title>
      </Helmet>
      <Skeleton>
        <Switch>
          <Route
            path={["/~settings/leap"]}
            render={() => {
              return (
                <LeapSettings api={props.api} />
              );
            }}
          />
          <Route
            path={["/~settings/notifications"]}
            render={() => {
              return (
                <NotificationPreferences
                  {...props}
                  graphConfig={props.notificationsGraphConfig}
                />
              );
            }}
          />
          <Route
            path={["/~settings/display"]}
            render={() => {
              return (
                <DisplayForm s3={props.s3} api={props.api} />
              );
            }}
          />
          <Route
            path="/~settings/s3"
            render={() => {
              return (
                <S3Form s3={props.s3} api={props.api} />
              );
            }}
          />
          <Route
            path="/~settings/calm"
            render={() => {
              return (
                <CalmPrefs api={props.api} />
              );
            }}
          />
          <Route
            path="/~settings/security"
            render={() => {
              return (
                <SecuritySettings api={props.api} />
              );
            }}
          />         
          <Route
            path={["/~settings"]}
            render={() => {
              return <Settings />;
            }}
          />
        </Switch>
      </Skeleton>
    </>
  );
}
