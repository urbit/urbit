import React, { ReactElement, useCallback } from 'react';
import { Formik, Form, FormikHelpers } from 'formik';
import * as Yup from 'yup';
import { RouteComponentProps } from 'react-router-dom';

import {
  Col,
  Box,
  Text,
  ManagedTextInputField as Input,
  ManagedCheckboxField as Checkbox
} from '@tlon/indigo-react';
import { Groups, Rolodex, GroupPolicy, Enc, Associations } from '@urbit/api';

import { AsyncButton } from '~/views/components/AsyncButton';
import { useWaitForProps } from '~/logic/lib/useWaitForProps';
import GlobalApi from '~/logic/api/global';
import { stringToSymbol } from '~/logic/lib/util';
import useGroupState from '~/logic/state/group';
import useMetadataState from '~/logic/state/metadata';

const formSchema = Yup.object({
  title: Yup.string().required('Group must have a name'),
  description: Yup.string(),
  isPrivate: Yup.boolean()
});

interface FormSchema {
  title: string;
  description: string;
  isPrivate: boolean;
}

interface NewGroupProps {
  api: GlobalApi;
}

export function NewGroup(props: NewGroupProps & RouteComponentProps): ReactElement {
  const { api, history } = props;
  const initialValues: FormSchema = {
    title: '',
    description: '',
    isPrivate: false
  };

  const groups = useGroupState(state => state.groups);
  const associations = useMetadataState(state => state.associations);
  const waiter = useWaitForProps({ groups, associations });

  const onSubmit = useCallback(
    async (values: FormSchema, actions: FormikHelpers<FormSchema>) => {
      try {
        const { title, description, isPrivate } = values;
        const name = stringToSymbol(title.trim());
        const policy: Enc<GroupPolicy> = isPrivate
          ? {
              invite: {
                pending: []
              }
            }
          : {
              open: {
                banRanks: [],
                banned: []
              }
            };
        await api.groups.create(name, policy, title, description);
        const path = `/ship/~${window.ship}/${name}`;
        await waiter((p) => {
          return path in p.groups && path in p.associations.groups;
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
    <>
      <Col overflowY="auto" p="3">
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
                caption="Anyone can join a public group. A private group is only joinable by invite."
              />
              <AsyncButton>Create Group</AsyncButton>
            </Col>
          </Form>
        </Formik>
      </Col>
    </>
  );
}
