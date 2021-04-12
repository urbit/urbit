import React, { useEffect } from "react";
import { AsyncButton } from "../../../components/AsyncButton";
import * as Yup from "yup";
import {
  Box,
  ManagedTextInputField as Input,
  ManagedCheckboxField as Checkbox,
  Col,
  Button,
  Center,
} from "@tlon/indigo-react";
import { Formik, Form, useFormikContext, FormikHelpers } from "formik";
import GlobalApi from "~/logic/api/global";
import { Notebook } from "~/types/publish-update";
import { Contacts } from "@urbit/api/contacts";
import { FormError } from "~/views/components/FormError";
import { RouteComponentProps, useHistory } from "react-router-dom";
import {Association} from "@urbit/api";
import { uxToHex } from "~/logic/lib/util";

interface MetadataFormProps {
  host: string;
  book: string;
  association: Association;
  api: GlobalApi;
}

interface FormSchema {
  name: string;
  description: string;
}

const formSchema = Yup.object({
  name: Yup.string().required("Notebook must have a name"),
  description: Yup.string()
});

const ResetOnPropsChange = (props: { init: FormSchema; book: string }) => {
  const { resetForm } = useFormikContext<FormSchema>();
  useEffect(() => {
    resetForm({ values: props.init });
  }, [props.book]);

  return null;
};


export function MetadataForm(props: MetadataFormProps) {
  const { api, book } = props;
  const { metadata } = props.association || {};

  const initialValues: FormSchema = {
    name: metadata?.title,
    description: metadata?.description,
  };


  const onSubmit = async (
    values: FormSchema,
    actions: FormikHelpers<FormSchema>
  ) => {
    try {
      const { name, description } = values;
      await api.metadata.metadataAdd(
        "publish",
        props.association.resource,
        props.association.group,
        name,
        description,
        props.association.metadata["date-created"],,
        uxToHex(props.association.metadata.color)
      );
      actions.setStatus({ success: null });
    } catch (e) {
      console.log(e);
      actions.setStatus({ error: e.message });
    }
  };

  return (
    <Formik
      validationSchema={formSchema}
      initialValues={initialValues}
      onSubmit={onSubmit}
    >
      <Form style={{ display: "contents" }}>
          <Input
            id="name"
            label="Rename"
            caption="Change the name of this notebook"
          />
          <Input
            id="description"
            label="Change description"
            caption="Change the description of this notebook"
          />
          <ResetOnPropsChange init={initialValues} book={book} />
          <AsyncButton primary loadingText="Updating.." border>
            Save
          </AsyncButton>
          <FormError message="Failed to update settings" />
      </Form>
    </Formik>
  );
}
