import React from 'react';
import { Box, Col, Row, Text, BaseLabel, Label } from '@tlon/indigo-react';
import { Association, resourceFromPath, Group, PermVariation } from '@urbit/api';
import GlobalApi from '~/logic/api/global';
import { Formik, Form, FormikHelpers } from 'formik';
import {
  GroupFeedPermsInput
} from '../Home/Post/GroupFeedPerms';
import { FormSubmit } from '~/views/components/FormSubmit';
import { StatelessAsyncToggle } from '~/views/components/StatelessAsyncToggle';
import useMetadataState from '~/logic/state/metadata';

interface FormSchema {
  permissions: PermVariation;
}

export function GroupFeedSettings(props: {
  association: Association;
  group: Group;
  api: GlobalApi;
}) {
  const { association, api } = props;
  const resource = resourceFromPath(association.group);
  let feedResource = '';
  if (
    association?.metadata?.config &&
    'group' in association.metadata.config &&
    association.metadata.config.group !== null &&
    'resource' in association.metadata.config.group
    ) {
    feedResource = association.metadata.config.group.resource ?? '';
  }
  const feedAssoc = useMetadataState(s => s.associations.graph[feedResource]);
  const isEnabled = Boolean(feedResource);

  const vip = feedAssoc?.metadata?.vip || '';
  const toggleFeed = async (actions: any) => {
    if (isEnabled) {
      await api.graph.disableGroupFeed(resource);
    } else {
      await api.graph.enableGroupFeed(resource, vip.trim());
    }
  };
  const initialValues: FormSchema = {
    permissions: vip
  };

  const onSubmit = async (
    values: FormSchema,
    actions: FormikHelpers<FormSchema>
  ) => {
    await api.metadata.update(feedAssoc, { vip: values.permissions.trim() as PermVariation });

    actions.setStatus({ success: null });
  };
  return (
    <Formik initialValues={initialValues} onSubmit={onSubmit}>
      <Form>
        <Col p="4" gapY="4">
          <Text id="feed" fontSize="2" fontWeight="medium">
            Group Feed Settings
          </Text>
          <BaseLabel display="flex" cursor="pointer">
            <StatelessAsyncToggle selected={isEnabled} onClick={toggleFeed} />
            <Col>
              <Label>Enable Group Feed</Label>
              <Label gray mt="1">
                Disabling the Group Feed archives the content and is not
                viewable to anyone
              </Label>
            </Col>
          </BaseLabel>
          {isEnabled && (
            <>
              <GroupFeedPermsInput id="permissions" />
              <FormSubmit start>Update Permissions</FormSubmit>
            </>
          )}
        </Col>
      </Form>
    </Formik>
  );
}
