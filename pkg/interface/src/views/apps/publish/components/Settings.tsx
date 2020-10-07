import React, { useEffect } from "react";
import { Box, Col, Button, Label } from "@tlon/indigo-react";
import GlobalApi from "~/logic/api/global";
import { Notebook } from "~/types/publish-update";
import { Contacts } from "~/types/contact-update";

import { MetadataForm } from "./MetadataForm";
import { Groups, Associations } from "~/types";
import GroupifyForm from "./GroupifyForm";
import { useHistory } from "react-router-dom";

interface SettingsProps {
  host: string;
  book: string;
  notebook: Notebook;
  contacts: Contacts;
  groups: Groups;
  api: GlobalApi;
  associations: Associations;
}

const Divider = (props) => (
  <Box {...props} borderBottom={1} borderBottomColor="lightGray" />
);
export function Settings(props: SettingsProps) {
  const history = useHistory();
  const onDelete = async () => {
    await props.api.publish.delBook(props.book);
    history.push(this.props.baseUrl || '/~404');
  };
  const groupPath = props.notebook?.["writers-group-path"];

  const isUnmanaged = props.groups?.[groupPath]?.hidden || false;

  return (
    <Box
      mx="auto"
      maxWidth="300px"
      my={4}
      gridTemplateColumns="1fr"
      gridAutoRows="auto"
      display="grid"
      gridRowGap={5}
    >
      {isUnmanaged && (
        <>
          <GroupifyForm {...props} />
          <Divider mt={4} />
        </>
      )}
      <MetadataForm {...props} />
      <Divider />
      <Col mb={4}>
        <Label>Delete Notebook</Label>
        <Label gray mt="2">
          Permanently delete this notebook. (All current members will no longer
          see this notebook.)
        </Label>
        <Button mt="2" onClick={onDelete} destructive style={{ cursor: 'pointer' }}>
          Delete this notebook
        </Button>
      </Col>
    </Box>
  );
}

export default Settings;
