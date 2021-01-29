import React from "react";
import { Col, Label, Row, Button } from "@tlon/indigo-react";
import { useHistory } from "react-router-dom";

import GlobalApi from "~/logic/api/global";
import { Association } from "~/types";
import { resourceFromPath } from "~/logic/lib/group";
import { StatelessAsyncButton } from "~/views/components/StatelessAsyncButton";
import ModalButton from "~/views/apps/launch/components/ModalButton";

export function DeleteGroup(props: {
  owner: boolean;
  api: GlobalApi;
  association: Association;
}) {
  const history = useHistory();
  const onDelete = async () => {
    const name = props.association.group.split("/").pop();
    if (props.owner) {
      const shouldDelete =
        prompt(`To confirm deleting this group, type ${name}`) === name;
      if (!shouldDelete) return;
    }
    const resource = resourceFromPath(props.association.group);
    await props.api.groups.removeGroup(resource);
    history.push("/");
  };

  const action = props.owner ? "Archive" : "Leave";
  const description = props.owner
    ? "Permanently delete this group. (All current members will no longer see this group.)"
    : "You can rejoin if it is an open group, or if you are reinvited";

  const icon = props.owner ? "X" : "SignOut";
  return (
    <ModalButton
      ml="2"
      color="red"
      boxShadow="none"
      icon={icon}
      text={`${action} group`}
    >
      {(dismiss: () => void) => (
        <Col p="4">
          <Label>{action} Group</Label>
          <Label gray mt="2">
            {description}
          </Label>
          <Row mt="2" justifyContent="flex-end">
            <Button onClick={dismiss}>Cancel</Button>
            <StatelessAsyncButton
              name={`delete-${props.association.group}`}
              onClick={onDelete}
              ml="2"
              destructive
              primary
            >
              {action} {`"${props.association.metadata.title}"`}
            </StatelessAsyncButton>
          </Row>
        </Col>
      )}
    </ModalButton>
  );
}
