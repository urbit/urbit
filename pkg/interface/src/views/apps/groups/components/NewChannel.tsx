import React, { useCallback } from "react";
import {
  Box,
  ManagedTextInputField as Input,
  Col,
  ManagedRadioButtonField as Radio,
  Text,
} from "@tlon/indigo-react";
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
import { ShipSearch } from "~/views/components/ShipSearch";
import { Rolodex } from "~/types";

interface FormSchema {
  name: string;
  description: string;
  ships: string[];
  type: "chat" | "publish" | "links";
}

const formSchema = Yup.object({
  name: Yup.string().required("Channel must have a name"),
  description: Yup.string(),
  ships: Yup.array(Yup.string()),
  type: Yup.string().required("Must choose channel type"),
});

interface NewChannelProps {
  api: GlobalApi;
  associations: Associations;
  contacts: Rolodex;
  groups: Groups;
  group?: string;
}

const EMPTY_INVITE_POLICY = { invite: { pending: [] } };

export function NewChannel(props: NewChannelProps & RouteComponentProps) {
  const { history, api, group } = props;

  const waiter = useWaitForProps(props, 5000);

  const onSubmit = async (values: FormSchema, actions) => {
    const resId: string = stringToSymbol(values.name);
    try {
      const { name, description, type, ships } = values;
      switch (type) {
        case "chat":
          const appPath = `/~${window.ship}/${resId}`;
          const groupPath = group || `/ship${appPath}`;

          await api.chat.create(
            name,
            description,
            appPath,
            groupPath,
            EMPTY_INVITE_POLICY,
            ships.map((s) => `~${s}`),
            true,
            false
          );
          break;
        case "publish":
          await props.api.publish.newBook(resId, name, description, group);
          break;
        case "links":
          if (group) {
            await api.graph.createManagedGraph(
              resId,
              name,
              description,
              group,
              "link"
            );
          } else {
            await api.graph.createUnmanagedGraph(
              resId,
              name,
              description,
              EMPTY_INVITE_POLICY,
              "link"
            );
          }
          break;

        default:
          console.log("fallthrough");
      }

      if (!group) {
        await waiter((p) => !!p?.groups?.[`/ship/~${window.ship}/${resId}`]);
      }
      actions.setStatus({ success: null });
    } catch (e) {
      console.error(e);
      actions.setStatus({ error: "Channel creation failed" });
    }
  };
  return (
    <Col overflowY="auto" p={3}>
      <Box fontWeight="bold" mb={4} color="black">
        New Channel
      </Box>
      <Formik
        validationSchema={formSchema}
        initialValues={{
          type: "chat",
          name: "",
          description: "",
          group: "",
          ships: [],
        }}
        onSubmit={onSubmit}
      >
        <Form>
          <Box
            display="grid"
            gridTemplateRows="auto"
            gridRowGap={4}
            gridTemplateColumns="300px"
          >
            <Col gapY="2">
              <Box color="black" mb={2}>Channel Type</Box>
              <Radio label="Chat" id="chat" name="type" />
              <Radio label="Notebook" id="publish" name="type" />
              <Radio label="Collection" id="links" name="type" />
            </Col>
            <Input
              id="name"
              label="Name"
              caption="Provide a name for your channel"
              placeholder="eg. My Channel"
            />
            <Input
              id="description"
              label="Description"
              caption="What's your channel about?"
              placeholder="Channel description"
            />
            <ShipSearch
              groups={props.groups}
              contacts={props.contacts}
              id="ships"
              label="Invitees"
            />
            <Box justifySelf="start">
              <AsyncButton
                primary
                loadingText="Creating..."
                type="submit"
                border
              >
                Create Channel
              </AsyncButton>
            </Box>
            <FormError message="Channel creation failed" />
          </Box>
        </Form>
      </Formik>
    </Col>
  );
}
