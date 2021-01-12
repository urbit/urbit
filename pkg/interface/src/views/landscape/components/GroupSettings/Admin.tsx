import React, { useEffect } from "react";
import { AsyncButton } from "~/views/components/AsyncButton";
import * as Yup from "yup";
import {
  Box,
  ManagedTextInputField as Input,
  ManagedToggleSwitchField as Checkbox,
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

interface FormSchema {
  title: string;
  description: string;
  color: string;
  isPrivate: boolean;
}

const formSchema = Yup.object({
  title: Yup.string().required("Group must have a name"),
  description: Yup.string(),
  color: Yup.string(),
  isPrivate: Yup.boolean(),
});

interface GroupAdminSettingsProps {
  group: Group;
  association: Association;
  api: GlobalApi;
}

export function GroupAdminSettings(props: GroupAdminSettingsProps) {
  const { group, association } = props;
  const { metadata } = association;
  const history = useHistory();
  const currentPrivate = "invite" in props.group.policy;
  const initialValues: FormSchema = {
    title: metadata?.title,
    description: metadata?.description,
    color: metadata?.color,
    isPrivate: currentPrivate,
  };

  const onSubmit = async (
    values: FormSchema,
    actions: FormikHelpers<FormSchema>
  ) => {
    try {
      const { title, description, color, isPrivate } = values;
      const uxColor = uxToHex(color);
      await props.api.metadata.update(props.association, {
        title,
        description,
        color: uxColor,
      });
      if (isPrivate !== currentPrivate) {
        const resource = resourceFromPath(props.association["group-path"]);
        const newPolicy: Enc<GroupPolicy> = isPrivate
          ? { invite: { pending: [] } }
          : { open: { banRanks: [], banned: [] } };
        const diff = { replace: newPolicy };
        await props.api.groups.changePolicy(resource, diff);
      }

      actions.setStatus({ success: null });
    } catch (e) {
      console.log(e);
      actions.setStatus({ error: e.message });
    }
  };

  const disabled =
    resourceFromPath(association["group-path"]).ship.slice(1) !== window.ship &&
    roleForShip(group, window.ship) !== "admin";

  return (
    <Formik
      validationSchema={formSchema}
      initialValues={initialValues}
      onSubmit={onSubmit}
    >
      <Form>
        <Col gapY={4}>
          <Input
            id="title"
            label="Group Name"
            caption="The name for your group to be called by"
            disabled={disabled}
          />
          <Input
            id="description"
            label="Group Description"
            caption="The description of your group"
            disabled={disabled}
          />
          <ColorInput
            id="color"
            label="Group color"
            caption="A color to represent your group"
            disabled={disabled}
          />
          <Checkbox
            id="isPrivate"
            label="Private group"
            caption="If enabled, users must be invited to join the group"
            disabled={disabled}
          />
          <AsyncButton
            disabled={disabled}
            primary
            loadingText="Updating.."
            border
          >
            Save
          </AsyncButton>
          <FormError message="Failed to update settings" />
        </Col>
      </Form>
    </Formik>
  );
}
