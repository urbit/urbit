import React, { useEffect } from "react";
import { AsyncButton } from "~/views/components/AsyncButton";
import * as Yup from "yup";
import {
  Box,
  ManagedTextInputField as Input,
  Col,
  Label,
  Text,
} from "@tlon/indigo-react";
import { Formik, Form, useFormikContext, FormikHelpers } from "formik";
import GlobalApi from "~/logic/api/global";
import { uxToHex } from "~/logic/lib/util";
import { FormError } from "~/views/components/FormError";
import { ColorInput } from "~/views/components/ColorInput";
import { Association, Groups, Associations } from "~/types";
import Writers from '~/views/apps/publish/components/Writers';
import GroupifyForm from "./GroupifyForm";

interface FormSchema {
  title: string;
  description: string;
  color: string;
}

interface ChannelSettingsProps {
  association: Association;
  groups: Groups;
  associations: Associations;
  api: GlobalApi;
}

export function ChannelSettings(props: ChannelSettingsProps) {
  const { api, association } = props;
  const { metadata } = association;
  const initialValues: FormSchema = {
    title: metadata?.title || "",
    description: metadata?.description || "",
    color: metadata?.color || "0x0",
  };

  const onSubmit = async (
    values: FormSchema,
    actions: FormikHelpers<FormSchema>
  ) => {
    try {
      const app = association["app-name"];
      const resource = association["app-path"];
      const group = association["group-path"];
      const date = metadata["date-created"];
      const { title, description, color } = values;
      await api.metadata.metadataAdd(
        app,
        resource,
        group,
        title,
        description,
        date,
        uxToHex(color),
        metadata.module
      );
      actions.setStatus({ success: null });
    } catch (e) {
      console.log(e);
      actions.setStatus({ error: e.message });
    }
  };

  return (
    <Col gapY="6" overflowY="auto" p={4}>
      <Formik initialValues={initialValues} onSubmit={onSubmit}>
        <Form style={{ display: "contents" }}>
          <Col flexShrink="0" maxWidth="512px" gapY="4">
            <Col mb={3}>
              <Text fontWeight="bold">Channel Settings</Text>
              <Label gray mt="2">
                Set the title, description and colour of the channel
              </Label>
            </Col>
            <Input
              id="title"
              label="Title"
              caption="Change the title of this channel"
            />
            <Input
              id="description"
              label="Change description"
              caption="Change the description of this channel"
            />
            <ColorInput
              id="color"
              label="Color"
              caption="Change the color of this channel"
            />
            <AsyncButton primary loadingText="Updating.." border>
              Save
            </AsyncButton>
            <FormError message="Failed to update settings" />
          </Col>
        </Form>
      </Formik>
      <Box borderBottom="1" borderBottomColor="lightGray" width="100%" maxWidth="512px" />
      {(metadata?.module === 'publish') && (<>
        <Writers {...props} />
        <Box borderBottom="1" borderBottomColor="lightGray" width="100%" maxWidth="512px" />
      </>)}
      <GroupifyForm {...props} />
    </Col>
  );
}
