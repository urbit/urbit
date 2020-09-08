import React, { useEffect } from "react";
import { Box, Col, Button, InputLabel, InputCaption } from "@tlon/indigo-react";
import GlobalApi from "~/logic/api/global";
import { Notebook } from "~/types/publish-update";
import { Contacts } from "~/types/contact-update";

import { MetadataForm } from "./MetadataForm";
import { Groups } from "~/types";

interface SettingsProps {
  host: string;
  book: string;
  notebook: Notebook;
  contacts: Contacts;
  groups: Groups;
  api: GlobalApi;
}

export function Settings(props: SettingsProps) {
  const onGroupify = () => {
    return props.api.publish.groupify(props.book);
  };

  const groupPath = props.notebook?.["writers-group-path"];

  const isUnmanaged = props.groups?.[groupPath]?.hidden || false;

  return (
    <Box
      mx="auto"
      maxWidth="300px"
      mb={4}
      gridTemplateColumns="1fr"
      gridAutoRows="auto"
      display="grid"
    >
      {isUnmanaged && (
        <>
          <Col mb={4}>
            <InputLabel>Groupify</InputLabel>
            <InputCaption>Turn this notebook into a group</InputCaption>
            <Button onClick={onGroupify} border>
              Groupify
            </Button>
          </Col>
          <Box mb={4} borderBottom={1} borderBottomColor="washedGray" />
        </>
      )}
      <MetadataForm {...props} />
    </Box>
  );
}

export default Settings;
