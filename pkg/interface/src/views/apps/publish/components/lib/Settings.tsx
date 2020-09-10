import React from "react";
import { Box, Col, Button, InputLabel, InputCaption } from "@tlon/indigo-react";
import GlobalApi from "~/logic/api/global";
import { MetadataForm } from "./MetadataForm";
import { Groups, Associations, Association, Contacts } from "~/types";
import GroupifyForm from "./GroupifyForm";
import { useHistory } from "react-router-dom";

interface SettingsProps {
  host: string;
  book: string;
  association: Association;
  contacts: Contacts;
  groups: Groups;
  api: GlobalApi;
  associations: Associations;
}

const Divider = (props) => (
  <Box {...props} mb={4} borderBottom={1} borderBottomColor="lightGray" />
);

export function Settings(props: SettingsProps) {
  const history = useHistory();
  const onDelete = async () => {
    await props.api.graph.deleteGraph(props.book)
    history.push("/~publish");
  };
  const groupPath = props.association?.['group-path'];

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
          <GroupifyForm {...props} />
          <Divider mt={4} />
        </>
      )}
      <MetadataForm {...props} />
      <Divider />
      <Col mb={4}>
        <InputLabel>Delete Notebook</InputLabel>
        <InputCaption>
          Permanently delete this notebook. (All current members will no longer
          see this notebook.)
        </InputCaption>
        <Button onClick={onDelete} mt={1} border error>
          Delete this notebook
        </Button>
      </Col>
    </Box>
  );
}

export default Settings;
