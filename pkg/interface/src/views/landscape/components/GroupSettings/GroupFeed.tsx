import { BaseLabel, Col, Label, Text } from '@tlon/indigo-react';
import { Association, createGroupFeed, disableGroupFeed, Group, metadataEdit, PermVariation, resourceFromPath } from '@urbit/api';
import { Form, Formik, FormikHelpers } from 'formik';
import React from 'react';
import useMetadataState from '~/logic/state/metadata';
import { FormSubmit } from '~/views/components/FormSubmit';
import { StatelessAsyncToggle } from '~/views/components/StatelessAsyncToggle';
import { GroupFeedPermsInput } from '../Home/Post/GroupFeedPerms';
import airlock from '~/logic/api';

interface FormSchema {
  permissions: PermVariation;
}

export function GroupFeedSettings(props: {
  association: Association;
  group: Group;
}) {
  const { association } = props;
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

  const vip = feedAssoc?.metadata?.vip || ' ';
  const toggleFeed = async (actions: any) => {
    if (isEnabled) {
      await airlock.thread(disableGroupFeed(resource));
    } else {
      await airlock.thread(createGroupFeed(resource, vip.trim()));
    }
  };
  const initialValues: FormSchema = {
    permissions: vip
  };

  const onSubmit = async (
    values: FormSchema,
    actions: FormikHelpers<FormSchema>
  ) => {
    await airlock.poke(
      metadataEdit(feedAssoc, {
        vip: values.permissions.trim() as PermVariation
      })
    );

    actions.setStatus({ success: null });
  };
  return (
    <Formik initialValues={initialValues} onSubmit={onSubmit}>
      <Form>
        <Col p={4} gapY={4}>
          <Text id="feed" fontSize={2} fontWeight="medium">
            Group Feed Settings
          </Text>
          <BaseLabel display="flex" cursor="pointer">
            <StatelessAsyncToggle selected={isEnabled} onClick={toggleFeed} />
            <Col>
              <Label>Enable Group Feed</Label>
              <Label gray mt={1}>
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
