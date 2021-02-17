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
  BaseLabel,
  Anchor,
  BaseAnchor
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



export function GroupPersonalSettings(props: {
  api: GlobalApi;
  association: Association;
  notificationsGroupConfig: GroupNotificationsConfig;
}) {

  const groupPath = props.association.group;

  const watching = props.notificationsGroupConfig.findIndex(g => g === groupPath) !== -1;

  const onClick = async () => {
    const func = !watching ? 'listenGroup' : 'ignoreGroup';
    await props.api.hark[func](groupPath);
  };

  return (
    <Col px="4" pb="4" gapY="4">
      <BaseAnchor pt="4" fontWeight="600" id="notifications" fontSize="2">Group Notifications</BaseAnchor>
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
    </Col>
  );
}
