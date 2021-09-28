import {
  Box, Col,

  ManagedCheckboxField as Checkbox, ManagedTextInputField as Input, Text
} from '@tlon/indigo-react';
import { createGroup, Enc, GroupPolicy } from '@urbit/api';
import { Form, Formik, FormikHelpers } from 'formik';
import React, { ReactElement, useCallback } from 'react';
import { useHistory } from 'react-router-dom';
import * as Yup from 'yup';
import { useWaitForProps } from '~/logic/lib/useWaitForProps';
import { stringToSymbol } from '~/logic/lib/util';
import useGroupState from '~/logic/state/group';
import useMetadataState from '~/logic/state/metadata';
import { AsyncButton } from '~/views/components/AsyncButton';
import airlock from '~/logic/api';

const formSchema = Yup.object({
  title: Yup.string()
    .matches(/^([a-zA-Z]|[\u2700-\u27bf]|(?:\ud83c[\udde6-\uddff]){2}|[\ud800-\udbff][\udc00-\udfff]|[\u0023-\u0039]\ufe0f?\u20e3|\u3299|\u3297|\u303d|\u3030|\u24c2|\ud83c[\udd70-\udd71]|\ud83c[\udd7e-\udd7f]|\ud83c\udd8e|\ud83c[\udd91-\udd9a]|\ud83c[\udde6-\uddff]|\ud83c[\ude01-\ude02]|\ud83c\ude1a|\ud83c\ude2f|\ud83c[\ude32-\ude3a]|\ud83c[\ude50-\ude51]|\u203c|\u2049|[\u25aa-\u25ab]|\u25b6|\u25c0|[\u25fb-\u25fe]|\u00a9|\u00ae|\u2122|\u2139|\ud83c\udc04|[\u2600-\u26FF]|\u2b05|\u2b06|\u2b07|\u2b1b|\u2b1c|\u2b50|\u2b55|\u231a|\u231b|\u2328|\u23cf|[\u23e9-\u23f3]|[\u23f8-\u23fa]|\ud83c\udccf|\u2934|\u2935|[\u2190-\u21ff]).*$/, 'Group names must start with letters or emoji')
    .required('Group must have a name'),
  description: Yup.string(),
  isPrivate: Yup.boolean()
});

interface FormSchema {
  title: string;
  description: string;
  isPrivate: boolean;
}

export function NewGroup(): ReactElement {
  const history = useHistory();
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
        await airlock.thread(createGroup(name, policy, title, description));
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
    [waiter, history]
  );

  return (
    <>
      <Col overflowY="auto" p={3}>
        <Box mb={3}>
          <Text fontWeight="bold">New Group</Text>
        </Box>
        <Formik
          validationSchema={formSchema}
          initialValues={initialValues}
          onSubmit={onSubmit}
        >
          <Form>
            <Col gapY={4}>
              <Input
                id="title"
                label="Name"
                caption="Provide a name for your group"
                placeholder="eg. My Group"
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
