import React from "react";

import { Row, Icon, Box, Col, Text } from "@tlon/indigo-react";

import GlobalApi from "~/logic/api/global";
import { StoreState } from "~/logic/store/type";
import DisplayForm from "./lib/DisplayForm";
import S3Form from "./lib/S3Form";
import SecuritySettings from "./lib/Security";
import RemoteContentForm from "./lib/RemoteContent";
import { NotificationPreferences } from "./lib/NotificationPref";
import { CalmPrefs } from "./lib/CalmPref";
import { Link } from "react-router-dom";

export function SettingsItem(props: {
  title: string;
  description: string;
  to: string;
}) {
  const { to, title, description } = props;
  return (
    <Link to={`/~settings/${to}`}>
      <Row alignItems="center" gapX="3">
        <Box
          borderRadius="2"
          backgroundColor="blue"
          width="64px"
          height="64px"
        />
        <Col gapY="2">
          <Text>{title}</Text>
          <Text gray>{description}</Text>
        </Col>
      </Row>
    </Link>
  );
}

export default function Settings(props: {}) {
  return (
    <Col gapY="5" p="5">
      <Col gapY="1">
        <Text fontSize="2">System Preferences</Text>
        <Text gray>Configure and customize Landscape</Text>
      </Col>
      <Box
        display="grid"
        width="100%"
        height="100%"
        gridTemplateColumns={["100%", "1fr 1fr"]}
        gridGap="3"
      >
        <SettingsItem
          to="notifications"
          title="Notifications"
          description="Set notification visibility and default behaviours for groups and messaging"
        />
        <SettingsItem
          to="display"
          title="Display"
          description="Customize visual interfaces across your Landscape"
        />
        <SettingsItem
          to="calm"
          title="CalmEngine"
          description="Modulate vearious elements across Landscape to maximize calmness"
        />
        <SettingsItem
          to="s3"
          title="Remote Storage"
          description="Configure S3-compatible storage solutions"
        />
        <SettingsItem
          to="security"
          title="Security"
          description="Manage sessions, login credentials, and Landscape access"
        />
        {/*
        <SettingsItem
          to="keyboard"
          title="Keyboard"
          description="Shortcuts, Keyboard Settings, Meta Key Assignments, etc."
        />
        <SettingsItem
          to="hosting"
          title="Hosting Services"
          description="Hosting-specific service configuration"
        />*/}
        <SettingsItem
          to="leap"
          title="Leap"
          description="Customize Leap ordering, omit modules or results"
        />
      </Box>
    </Col>
  );
}
