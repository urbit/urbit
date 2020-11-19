import React from "react";
import {
  Box,
  Button,
  ManagedCheckboxField as Checkbox,
} from "@tlon/indigo-react";
import { Formik, Form } from "formik";
import * as Yup from "yup";

import GlobalApi from "~/logic/api/global";
import { LocalUpdateRemoteContentPolicy } from "~/types/local-update";

const formSchema = Yup.object().shape({
  imageShown: Yup.boolean(),
  audioShown: Yup.boolean(),
  videoShown: Yup.boolean(),
  oembedShown: Yup.boolean(),
});

interface FormSchema {
  imageShown: boolean;
  audioShown: boolean;
  videoShown: boolean;
  oembedShown: boolean;
}

interface RemoteContentFormProps {
  api: GlobalApi;
  remoteContentPolicy: LocalUpdateRemoteContentPolicy;
}

export default function RemoteContentForm(props: RemoteContentFormProps) {
  const { api, remoteContentPolicy } = props;
  const imageShown = remoteContentPolicy.imageShown;
  const audioShown = remoteContentPolicy.audioShown;
  const videoShown = remoteContentPolicy.videoShown;
  const oembedShown = remoteContentPolicy.oembedShown;
  return (
    <Formik
      validationSchema={formSchema}
      initialValues={
        {
          imageShown,
          audioShown,
          videoShown,
          oembedShown,
        } as FormSchema
      }
      onSubmit={(values, actions) => {
        api.local.setRemoteContentPolicy({
          imageShown: values.imageShown,
          audioShown: values.audioShown,
          videoShown: values.videoShown,
          oembedShown: values.oembedShown,
        });
        api.local.dehydrate();
        actions.setSubmitting(false);
      }}
    >
      {(props) => (
        <Form>
          <Box
            display="grid"
            gridTemplateColumns="1fr"
            gridTemplateRows="audio"
            gridRowGap={5}
          >
            <Box color="black" fontSize={1} fontWeight={900}>
              Remote Content
            </Box>
            <Checkbox label="Load images" id="imageShown" />
            <Checkbox label="Load audio files" id="audioShown" />
            <Checkbox label="Load video files" id="videoShown" />
            <Checkbox
              label="Load embedded content"
              id="oembedShown"
              caption="Embedded content may contain scripts"
            />
            <Button style={{ cursor: 'pointer' }} border={1} borderColor="washedGray" type="submit">
              Save
            </Button>
          </Box>
        </Form>
      )}
    </Formik>
  );
}

