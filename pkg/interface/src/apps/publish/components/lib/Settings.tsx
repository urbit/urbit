import React, { useEffect } from "react";
import { AsyncButton } from "../../../../components/AsyncButton";
import * as Yup from "yup";
import {
  Box,
  Input,
  Checkbox,
  Col,
  InputLabel,
  InputCaption,
  Button,
  Center,
} from "@tlon/indigo-react";
import { Formik, Form, useFormikContext, FormikHelpers } from "formik";
import GlobalApi from "../../../../api/global";
import { Notebook } from "../../../../types/publish-update";
import { Contacts } from "../../../../types/contact-update";
import { FormError } from "../../../../components/FormError";

interface SettingsProps {
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

export function Settings(props: SettingsProps) {
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
      await api.publish.publishAction({
        "edit-book": {
          book,
          title: values.name,
          about: values.description,
          coms: values.comments,
          group: null,
        },
      });
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
              longer see this notebook
            </InputCaption>
            <Button mt={1} border error>
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
          <Checkbox
            id="comments"
            label="Comments"
            caption="Subscribers may comment when enabled"
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
