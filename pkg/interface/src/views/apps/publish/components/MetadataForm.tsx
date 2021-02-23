import React, { useEffect } from "react";
import * as Yup from "yup";

import {
  ManagedTextInputField as Input,
} from "@tlon/indigo-react";
import {Association, uxToHex, Contacts, metadataAdd } from "@urbit/api";

import { Formik, Form, useFormikContext, FormikHelpers } from "formik";
import GlobalApi from "~/logic/api/global";
import { FormError } from "~/views/components/FormError";
import { AsyncButton } from "~/views/components/AsyncButton";
import useApi from "~/logic/lib/useApi";

interface MetadataFormProps {
  host: string;
  book: string;
  association: Association;
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
  const { book } = props;
  const { metadata } = props.association || {};
  const api = useApi();

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
      await api.poke(metadataAdd(
        window.ship,
        "publish",
        props.association.resource,
        // TODO there should be a path here
        props.association.group,
        name,
        description,
        props.association.metadata["date-created"],,
        uxToHex(props.association.metadata.color)
      ));
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
