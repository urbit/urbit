import React, { useEffect } from "react";
import { AsyncButton } from "../../../../components/AsyncButton";
import * as Yup from "yup";
import {
  Box,
  Input,
  Col,
  InputLabel,
  InputCaption,
  Button,
} from "@tlon/indigo-react";
import { Formik, Form, useFormikContext, FormikHelpers } from "formik";
import GlobalApi from "~/logic/api/global";
import { Contacts } from "~/types/contact-update";
import { FormError } from "~/views/components/FormError";
import { useHistory } from "react-router-dom";
import { Association } from "~/types";
import { uxToHex } from "~/logic/lib/util";

interface SettingsProps {
  host: string;
  book: string;
  association: Association;
  contacts: Contacts;
  api: GlobalApi;
}

interface FormSchema {
  name: string;
  description: string;
}

const formSchema = Yup.object({
  name: Yup.string().required("Notebook must have a name"),
  description: Yup.string(),
});

const ResetOnPropsChange = (props: { init: FormSchema; book: string }) => {
  const { resetForm } = useFormikContext<FormSchema>();
  useEffect(() => {
    resetForm({ values: props.init });
  }, [props.book]);

  return null;
};

export function Settings(props: SettingsProps) {
  const { api, book } = props;
  const history = useHistory();
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
        props.association["app-path"],
        props.association["group-path"],
        name,
        description,
        props.association.metadata["date-created"],
        uxToHex(props.association.metadata.color)
      );
      actions.setStatus({ success: null });
    } catch (e) {
      console.log(e);
      actions.setStatus({ error: e.message });
    }
  };

  const onDelete = async () => {
    await api.graph.deleteGraph(book);
    history.push("/~publish");
  };

  return (
    <Formik
      validationSchema={formSchema}
      initialValues={initialValues}
      onSubmit={onSubmit}
    >
      <Form>
        <Box
          maxWidth="300px"
          mb={4}
          gridTemplateColumns="1fr"
          gridAutoRows="auto"
          display="grid"
        >
          <Col mb={4}>
            <InputLabel>Delete Notebook</InputLabel>
            <InputCaption>
              Permanently delete this notebook. (All current members will no
              longer see this notebook.)
            </InputCaption>
            <Button type="button" onClick={onDelete} mt={1} border error>
              Delete this notebook
            </Button>
          </Col>
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
          <AsyncButton loadingText="Updating.." border>
            Save
          </AsyncButton>
          <FormError message="Failed to update settings" />
        </Box>
      </Form>
    </Formik>
  );
}

export default Settings;
