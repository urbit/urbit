import React, { useEffect } from "react";
import { AsyncButton } from "../../../../components/AsyncButton";
import * as Yup from "yup";
import {
  Box,
  ManagedTextInputField as Input,
  ManagedCheckboxField as Checkbox,
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
import { resourceFromPath } from "~/logic/lib/group";

interface FormSchema {
  name: string;
  description?: string;
  isPrivate: boolean;
}

const formSchema = Yup.object({
  name: Yup.string().required("Group must have a name"),
  description: Yup.string(),
  isPrivate: Yup.boolean(),
});

interface GroupSettingsProps {
  group: Group;
  association: Association;
  api: GlobalApi;
}
export function GroupSettings(props: GroupSettingsProps) {
  const { metadata } = props.association;
  const currentPrivate = "invite" in props.group.policy;
  const initialValues: FormSchema = {
    name: metadata.title,
    description: metadata.description,
    isPrivate: currentPrivate,
  };

  const onSubmit = async (
    values: FormSchema,
    actions: FormikHelpers<FormSchema>
  ) => {
    try {
      const { name, description, isPrivate } = values;
      await props.api.metadata.editGroup(props.association, name, description);
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

  const onDelete = async () => {};

  return (
    <Box height="100%" overflowY="auto">
      <Formik
        validationSchema={formSchema}
        initialValues={initialValues}
        onSubmit={onSubmit}
      >
        <Form style={{ display: "contents" }}>
          <Box
            maxWidth="300px"
            gridTemplateColumns="1fr"
            gridAutoRows="auto"
            display="grid"
            my={3}
            mx={4}
          >
            <Col mb={4}>
              <Label>Delete Group</Label>
              <Label gray mt="2">
                Permanently delete this group. (All current members will no
                longer see this group.)
              </Label>
              <Button onClick={onDelete} mt={1} destructive>
                Delete this group
              </Button>
            </Col>
            <Box mb={4} borderBottom={1} borderBottomColor="washedGray" />
            <Input
              id="name"
              label="Group Name"
              caption="The name for your group to be called by"
            />
            <Input
              id="description"
              label="Group Description"
              caption="The description of your group"
            />
            <Checkbox
              id="isPrivate"
              label="Private group"
              caption="If enabled, users must be invited to join the group"
            />
            <AsyncButton primary loadingText="Updating.." border>
              Save
            </AsyncButton>
            <FormError message="Failed to update settings" />
          </Box>
        </Form>
      </Formik>
    </Box>
  );
}
