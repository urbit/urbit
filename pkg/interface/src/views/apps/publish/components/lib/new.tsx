import React, { useCallback } from "react";
import { Box, Input, Col } from "@tlon/indigo-react";
import { Formik, Form } from "formik";
import * as Yup from "yup";
import GlobalApi from "~/logic/api/global";
import { AsyncButton } from "~/views/components/AsyncButton";
import { FormError } from "~/views/components/FormError";
import { RouteComponentProps } from "react-router-dom";
import { stringToSymbol } from "~/logic/lib/util";
import GroupSearch from "~/views/components/GroupSearch";
import { Associations } from "~/types/metadata-update";
import { useWaitForProps } from "~/logic/lib/useWaitForProps";
import { Notebooks } from "~/types/publish-update";
import { Groups } from "~/types/group-update";

interface FormSchema {
  name: string;
  description: string;
  group?: string;
}

const formSchema = Yup.object({
  name: Yup.string().required("Notebook must have a name"),
  description: Yup.string(),
  group: Yup.string(),
});

interface NewScreenProps {
  api: GlobalApi;
  associations: Associations;
  notebooks: Notebooks;
  groups: Groups;
}

export function NewScreen(props: NewScreenProps & RouteComponentProps) {
  const { history } = props;

  const waiter = useWaitForProps(props, 5000);

  const onSubmit = async (values: FormSchema, actions) => {
    const bookId = stringToSymbol(values.name);
    try {
      const { name, description, group } = values;
      await props.api.publish.newBook(bookId, name, description, group);
      await waiter((p) => !!p?.notebooks?.[`~${window.ship}`]?.[bookId]);
      if (!group) {
        await waiter((p) => !!p?.groups?.[`/ship/~${window.ship}/${bookId}`]);
      }
      actions.setStatus({ success: null });
      history.push(`/~publish/notebook/~${window.ship}/${bookId}`);
    } catch (e) {
      console.error(e);
      actions.setStatus({ error: "Notebook creation failed" });
    }
  };
  return (
    <Col p={3}>
      <Box mb={4} color="black">New Notebook</Box>
      <Formik
        validationSchema={formSchema}
        initialValues={{ name: "", description: "", group: "" }}
        onSubmit={onSubmit}
      >
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
            <FormError message="Notebook Creation failed" />
          </Box>
        </Form>
      </Formik>
    </Col>
  );
}

export default NewScreen;
