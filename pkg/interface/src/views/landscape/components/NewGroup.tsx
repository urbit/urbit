import React, { useState, useCallback } from "react";
import { Body } from "~/views/components/Body";
import {
  Col,
  Box,
  Text,
  ManagedTextInputField as Input,
  ManagedCheckboxField as Checkbox
} from "@tlon/indigo-react";
import { Formik, Form, FormikHelpers } from "formik";
import { AsyncButton } from "~/views/components/AsyncButton";
import * as Yup from "yup";
import { Groups, Rolodex, GroupPolicy, Enc } from "~/types";
import { useWaitForProps } from "~/logic/lib/useWaitForProps";
import GlobalApi from "~/logic/api/global";
import { stringToSymbol } from "~/logic/lib/util";
import {RouteComponentProps} from "react-router-dom";

const formSchema = Yup.object({
  title: Yup.string().required("Group must have a name"),
  description: Yup.string(),
  isPrivate: Yup.boolean(),
});

interface FormSchema {
  title: string;
  description: string;
  isPrivate: boolean;
}

interface NewGroupProps {
  groups: Groups;
  contacts: Rolodex;
  api: GlobalApi;
}

export function NewGroup(props: NewGroupProps & RouteComponentProps) {
  const { api, history } = props;
  const initialValues: FormSchema = {
    title: "",
    description: "",
    isPrivate: false,
  };

  const waiter = useWaitForProps(props);

  const onSubmit = useCallback(
    async (values: FormSchema, actions: FormikHelpers<FormSchema>) => {
      try {
        const { title, description, isPrivate } = values;
        const name = stringToSymbol(title.trim());
        const policy: Enc<GroupPolicy> = isPrivate
          ? {
              invite: {
                pending: [],
              },
            }
          : {
              open: {
                banRanks: [],
                banned: [],
              },
            };
        await api.contacts.create(name, policy, title, description);
        const path = `/ship/~${window.ship}/${name}`;
        await waiter(({ contacts, groups }) => {
          return path in contacts && path in groups;
        });

        actions.setStatus({ success: null });
        history.push(`/~landscape${path}`);
      } catch (e) {
        console.error(e);
        actions.setStatus({ error: e.message });
      }
    },
    [api, waiter, history]
  );

  return (
    <Body>
      <Col maxWidth="300px" overflowY="auto" p="3">
        <Box mb={3}>
          <Text fontWeight="bold">New Group</Text>
        </Box>
        <Formik
          validationSchema={formSchema}
          initialValues={initialValues}
          onSubmit={onSubmit}
        >
          <Form>
            <Col gapY="4">
              <Input
                id="title"
                label="Name"
                caption="Provide a name for your group"
                placeholder="eg. My Channel"
              />
              <Input
                id="description"
                label="Description"
                caption="What's your group about?"
                placeholder="Group description"
              />
              <Checkbox
                id="isPrivate"
                label="Private Group"
                caption="Is your group private?"
              />
              <AsyncButton>Create Group</AsyncButton>
            </Col>
          </Form>
        </Formik>
      </Col>
    </Body>
  );
}
