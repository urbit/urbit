import React, { useState, useCallback } from "react";
import { Body } from "~/views/components/Body";
import {
  Col,
  Box,
  Text,
  ManagedTextInputField as Input
} from "@tlon/indigo-react";
import { Formik, Form, FormikHelpers } from "formik";
import { AsyncButton } from "~/views/components/AsyncButton";
import * as Yup from "yup";
import { Groups, Rolodex } from "~/types";
import { useWaitForProps } from "~/logic/lib/useWaitForProps";
import GlobalApi from "~/logic/api/global";
import { RouteComponentProps } from "react-router-dom";
import urbitOb from "urbit-ob";

const formSchema = Yup.object({
  group: Yup.string()
    .required("Must provide group to join")
    .test("is-valid", "Invalid group", (group: string | null | undefined) => {
      if (!group) {
        return false;
      }
      const [patp, name] = group.split("/");
      return urbitOb.isValidPatp(patp) && name.length > 0;
    }),
});

interface FormSchema {
  group: string;
}

interface JoinGroupProps {
  groups: Groups;
  contacts: Rolodex;
  api: GlobalApi;
}

export function JoinGroup(props: JoinGroupProps & RouteComponentProps) {
  const { api, history } = props;
  const initialValues: FormSchema = {
    group: "",
  };

  const waiter = useWaitForProps(props);

  const onSubmit = useCallback(
    async (values: FormSchema, actions: FormikHelpers<FormSchema>) => {
      try {
        const [ship, name] = values.group.split("/");
        await api.contacts.join({ ship, name });
        const path = `/ship/${ship}/${name}`;
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
          <Text fontWeight="bold">Join Group</Text>
        </Box>
        <Formik
          validationSchema={formSchema}
          initialValues={initialValues}
          onSubmit={onSubmit}
        >
          <Form>
            <Col gapY="4">
              <Input
                id="group"
                label="Group"
                caption="What group are you joining?"
                placeholder="~sampel-palnet/test-group"
              />
              <AsyncButton>Join Group</AsyncButton>
            </Col>
          </Form>
        </Formik>
      </Col>
    </Body>
  );
}
