import React, { Component, useCallback } from "react";
import {
  Box,
  Input,
  Text,
  Button,
  Col,
  ErrorMessage,
} from "@tlon/indigo-react";
import { Formik, Form } from "formik";
import * as Yup from "yup";
import GlobalApi from "../../../../api/global";
import DropdownSearch, {
  InviteSearch,
} from "../../../../components/InviteSearch";
import { AsyncButton } from "../../../../components/AsyncButton";
import { Link, RouteComponentProps } from "react-router-dom";
import { stringToSymbol } from "../../../../lib/util";
import GroupSearch from "../../../../components/GroupSearch";
import { Associations } from "../../../../types/metadata-update";
import { useWaitForProps } from "../../../../lib/useWaitForProps";
import { Notebooks } from "../../../../types/publish-update";

interface FormSchema {
  name: string;
  description: string;
  group: string;
}

const formSchema = Yup.object({
  name: Yup.string().required("Notebook must have a name"),
  description: Yup.string(),
  group: Yup.string().required("Notebook must be part of a group"),
});

interface NewScreenProps {
  api: GlobalApi;
  associations: Associations;
  notebooks: Notebooks;
}

export function NewScreen(props: NewScreenProps & RouteComponentProps) {
  const { api, history } = props;
  const waiter = useWaitForProps(props, 5000);

  const onSubmit = useCallback(
    async (values: FormSchema, actions) => {
      const bookId = stringToSymbol(values.name);
      const groupInfo = {
        "group-path": values.group,
        invitees: [],
        "use-preexisting": true,
        "make-managed": true,
      };

      const action = {
        "new-book": {
          book: bookId,
          title: values.name,
          about: values.description,
          coms: true,
          group: groupInfo,
        },
      };
      try {
        await props.api.publish.publishAction(action);
        await waiter((p) => !!p?.notebooks?.[`~${window.ship}`]?.[bookId]);
        actions.setSubmitting(false);
        actions.setStatus({ success: null });
        history.push(`/~publish/notebook/~${window.ship}/${bookId}`);
      } catch (e) {
        console.error(e);
        actions.setSubmitting(false);
        actions.setStatus({ error: "Notebook creation failed" });
      }
    },
    [api]
  );
  return (
    <Col p={3}>
      <Box mb={4}>New Notebook</Box>
      <Formik
        validationSchema={formSchema}
        initialValues={{ name: "", description: "", group: "" }}
        onSubmit={onSubmit}
      >
        {({ isSubmitting, status }) => (
          <Form>
            <Box
              display="grid"
              gridTemplateRows="auto"
              gridRowGap={2}
              gridTemplateColumns="300px"
            >
              <Input
                id="name"
                label="Name"
                caption="Provide a name for your notebook"
                placeholder="eg. My Journal"
              />
              <Input
                id="description"
                label="Description"
                caption="What's your notebook about?"
                placeholder="Notebook description"
              />
              <GroupSearch
                id="group"
                label="Group"
                caption="What group is the notebook for?"
                associations={props.associations}
              />

              <Box justifySelf="start">
                <AsyncButton loadingText="Creating..." type="submit" border>
                  Create Notebook
                </AsyncButton>
              </Box>
              {status && status.error && (
                <ErrorMessage>{status.error}</ErrorMessage>
              )}
            </Box>
          </Form>
        )}
      </Formik>
    </Col>
  );
}

export default NewScreen;
