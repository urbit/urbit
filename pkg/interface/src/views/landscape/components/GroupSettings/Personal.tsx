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
} from "@tlon/indigo-react";
import { Formik, Form, useFormikContext, FormikHelpers } from "formik";
import { FormError } from "~/views/components/FormError";
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

function DeleteGroup(props: {
  owner: boolean;
  api: GlobalApi;
  association: Association;
}) {
  const history = useHistory();
  const onDelete = async () => {
    await props.api.contacts.delete(props.association["group-path"]);
    history.push("/");
  };

  const action = props.owner ? "Delete" : "Leave";
  const description = props.owner
    ? "Permanently delete this group. (All current members will no longer see this group.)"
    : "Leave this group. You can rejoin if it is an open group, or if you are reinvited";

  return (
    <Col>
      <Label>{action} Group</Label>
      <Label gray mt="2">
        {description}
      </Label>
      <StatelessAsyncButton onClick={onDelete} mt={2} destructive>
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

  const initialValues: FormSchema = {
    watching
  };
  const onSubmit = async (values: FormSchema) => {
    if(values.watching === watching) {
      return;
    }
    const func = values.watching ? 'listenGroup' : 'ignoreGroup';
    await props.api.hark[func](groupPath);
  };

  return (
    <Col gapY="4">
      <FormikOnBlur initialValues={initialValues} onSubmit={onSubmit}>
        <Toggle
          id="watching"
          label="Notify me on group activity"
          caption="Send me notifications when this group changes"
        />
      </FormikOnBlur>
      <DeleteGroup association={props.association} owner api={props.api} />
    </Col>
  );
}
