import React, { useState } from "react";
import {
  Box,
  Text,
  Button,
  Col,
  StatelessCheckboxField,
} from "@tlon/indigo-react";

import GlobalApi from "~/logic/api/global";
import { BackButton } from "./BackButton";

interface SecuritySettingsProps {
  api: GlobalApi;
}

export default function SecuritySettings({ api }: SecuritySettingsProps) {
  const [allSessions, setAllSessions] = useState(false);
  return (
    <>
    <BackButton/>
    <Col gapY="5" p="5" pt="4">
      <Col gapY="1" mt="0">
        <Text fontSize={2} fontWeight="medium">
          Security Preferences
        </Text>
        <Text gray>
          Manage sessions, login credentials and Landscape access
        </Text>
      </Col>
      <Col gapY="1">
        <Text color="black">
          Log out of this session
        </Text>
        <Text mb="3" gray>
          {allSessions
            ? "You will be logged out of all browsers that have currently logged into your Urbit."
            : "You will be logged out of your Urbit on this browser."}
        </Text>
        <StatelessCheckboxField
          mb="3"
          selected={allSessions}
          onChange={() => setAllSessions((s) => !s)}
        >
          <Text>Log out of all sessions</Text>
        </StatelessCheckboxField>
        <form method="post" action="/~/logout">
          {allSessions && <input type="hidden" name="all" />}
          <Button
            primary
            destructive
            border={1}
            style={{ cursor: "pointer" }}
          >
            Logout
          </Button>
        </form>
      </Col>
    </Col>
    </>
  );
}
