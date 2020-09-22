import React, { useCallback } from "react";
import { RouteComponentProps } from "react-router-dom";
import { Box, Input, Col } from "@tlon/indigo-react";
import { Formik, Form } from "formik";
import * as Yup from "yup";

import { AsyncButton } from "~/views/components/AsyncButton";
import { FormError } from "~/views/components/FormError";
import GroupSearch from "~/views/components/GroupSearch";

import GlobalApi from "~/logic/api/global";
import { stringToSymbol } from "~/logic/lib/util";
import { useWaitForProps } from "~/logic/lib/useWaitForProps";

import { Associations } from "~/types/metadata-update";
import { Notebooks } from "~/types/publish-update";
import { Groups, GroupPolicy } from "~/types/group-update";

const formSchema = Yup.object({
  name: Yup.string().required("Collection must have a name"),
  description: Yup.string(),
  group: Yup.string(),
});


export function NewScreen(props: object) {
  const { history, api } = props;
  const waiter = useWaitForProps(props, 5000);

  const onSubmit = async (values: object, actions) => {
    const resourceId = stringToSymbol(values.name);
    try {
      const { name, description, group } = values;
      if (!!group) {
        await props.api.graph.createManagedGraph(
          resourceId,
          name,
          description,
          group,
          "link"
        );
      } else {
        await props.api.graph.createUnmanagedGraph(
          resourceId,
          name,
          description,
          { invite: { pending: [] } },
          "link"
        );
      }

      await waiter((p) => p?.graphKeys?.has(`${window.ship}/${resourceId}`));
      actions.setStatus({ success: null });
      history.push(`/~link/${window.ship}/${resourceId}`);
    } catch (e) {
      console.error(e);
      actions.setStatus({ error: "Collection creation failed" });
    }
  };

  return (
    <Col p={3}>
      <Box mb={4} color="black">New Collection</Box>
      <Formik
        validationSchema={formSchema}
        initialValues={{ name: "", description: "", group: "" }}
        onSubmit={onSubmit}>
        <Form>
          <Box
            display="grid"
            gridTemplateRows="auto"
            gridRowGap={2}
            gridTemplateColumns="300px">
            <Input
              id="name"
              label="Name"
              caption="Provide a name for your collection"
              placeholder="eg. My Links"
            />
            <Input
              id="description"
              label="Description"
              caption="What's your collection about?"
              placeholder="Collection description"
            />
            <GroupSearch
              id="group"
              label="Group"
              caption="What group is the collection for?"
              associations={props.associations}
            />
            <Box justifySelf="start">
              <AsyncButton loadingText="Creating..." type="submit" border>
                Create Collection
              </AsyncButton>
            </Box>
            <FormError message="Collection creation failed" />
          </Box>
        </Form>
      </Formik>
    </Col>
  );
}

export default NewScreen;
