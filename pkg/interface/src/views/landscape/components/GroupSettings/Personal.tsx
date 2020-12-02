import React, { useCallback } from "react";

import { AsyncButton } from "~/views/components/AsyncButton";
import * as Yup from "yup";
import {
  Box,
  ManagedTextInputField as Input,
  ManagedToggleSwitchField as Toggle,
  Col,
  Label,
  Button,
  LoadingSpinner,
  BaseLabel
} from "@tlon/indigo-react";
import { Group, GroupPolicy } from "~/types/group-update";
import { Enc } from "~/types/noun";
import { Association } from "~/types/metadata-update";
import GlobalApi from "~/logic/api/global";
import { resourceFromPath, roleForShip } from "~/logic/lib/group";
import { StatelessAsyncButton } from "~/views/components/StatelessAsyncButton";
import { ColorInput } from "~/views/components/ColorInput";
import { useHistory } from "react-router-dom";

import { uxToHex } from "~/logic/lib/util";
import { FormikOnBlur } from "~/views/components/FormikOnBlur";
import {GroupNotificationsConfig} from "~/types";
import {StatelessAsyncToggle} from "~/views/components/StatelessAsyncToggle";

function DeleteGroup(props: {
  owner: boolean;
  api: GlobalApi;
  association: Association;
}) {
  const history = useHistory();
  const onDelete = async () => {
    const name = props.association['group-path'].split('/').pop();
    if (props.owner) {
      const shouldDelete = (prompt(`To confirm deleting this group, type ${name}`) === name);
      if (!shouldDelete) return;
    }
    const resource = resourceFromPath(props.association["group-path"])
    await props.api.groups.removeGroup(resource);
    history.push("/");
  };

  const action = props.owner ? "Delete" : "Leave";
  const description = props.owner
    ? "Permanently delete this group. (All current members will no longer see this group.)"
    : "You can rejoin if it is an open group, or if you are reinvited";

  return (
    <Col>
      <Label>{action} Group</Label>
      <Label gray mt="2">
        {description}
      </Label>
      <StatelessAsyncButton onClick={onDelete} mt={2} destructive={props.owner}>
        {action} this group
      </StatelessAsyncButton>
    </Col>
  );
}

interface FormSchema {
  watching: boolean;
}

export function GroupPersonalSettings(props: {
  api: GlobalApi;
  association: Association;
  notificationsGroupConfig: GroupNotificationsConfig;
}) {

  const groupPath = props.association['group-path'];

  const watching = props.notificationsGroupConfig.findIndex(g => g === groupPath) !== -1;

  const onClick = async () => {
    const func = !watching ? 'listenGroup' : 'ignoreGroup';
    await props.api.hark[func](groupPath);
  };

  const owner = (props.group?.tags?.role?.admin.has(window.ship) || false);

  return (
    <Col gapY="4">
      <BaseLabel 
        htmlFor="asyncToggle"
        display="flex"
        cursor="pointer"
      >
        <StatelessAsyncToggle selected={watching} onClick={onClick} />
        <Col>
          <Label>Notify me on group activity</Label>
          <Label mt="2" gray>Send me notifications when this group changes</Label>
        </Col>
      </BaseLabel>
      <DeleteGroup association={props.association} owner={owner} api={props.api} />
    </Col>
  );
}
