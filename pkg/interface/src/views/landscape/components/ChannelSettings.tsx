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
import { uxToHex } from '~/logic/lib/util';
import { FormError } from "~/views/components/FormError";
import { ColorInput } from "~/views/components/ColorInput";
import { Association } from "~/types";

interface FormSchema {
  title: string;
  description: string;
  color: string;
}

interface ChannelSettingsProps {
  association: Association;
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
    <Box overflowY="auto" p={4}>
      <Formik initialValues={initialValues} onSubmit={onSubmit}>
        <Form style={{ display: "contents" }}>
          <Box
            display="grid"
            gridTemplateColumns="100%"
            maxWidth="512px"
            gridAutoRows="auto"
            width="100%"
            gridRowGap={4}
          >
            <Col mb={3}>
              <Text fontWeight="bold">Channel Settings</Text>
              <Label gray mt='2'>
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
          </Box>
        </Form>
      </Formik>
    </Box>
  );
}
