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
import { Contacts } from "~/types/contact-update";
import { FormError } from "~/views/components/FormError";
import { RouteComponentProps, useHistory } from "react-router-dom";

interface MetadataFormProps {
  host: string;
  book: string;
  notebook: Notebook;
  contacts: Contacts;
  api: GlobalApi;
}

interface FormSchema {
  name: string;
  description: string;
  comments: boolean;
}

const formSchema = Yup.object({
  name: Yup.string().required("Notebook must have a name"),
  description: Yup.string(),
  comments: Yup.boolean(),
});

const ResetOnPropsChange = (props: { init: FormSchema; book: string }) => {
  const { resetForm } = useFormikContext<FormSchema>();
  useEffect(() => {
    resetForm({ values: props.init });
  }, [props.book]);

  return null;
};


export function MetadataForm(props: MetadataFormProps) {
  const { host, notebook, api, book } = props;
  const initialValues: FormSchema = {
    name: notebook?.title,
    description: notebook?.about,
    comments: notebook?.comments,
  };

  const onSubmit = async (
    values: FormSchema,
    actions: FormikHelpers<FormSchema>
  ) => {
    try {
      const { name, description, comments } = values;
      await api.publish.editBook(book, name, description, comments);
      api.publish.fetchNotebook(host, book);
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
          <Checkbox
            id="comments"
            label="Comments"
            caption="Subscribers may comment when enabled"
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
