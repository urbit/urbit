import React from "react";
import { Box, Col, Row, Text, BaseLabel, Label } from "@tlon/indigo-react";
import { Association, resourceFromPath, Group } from "@urbit/api";
import GlobalApi from "~/logic/api/global";
import { Formik, Form, FormikHelpers } from "formik";
import {
  GroupFeedPermissions,
  GroupFeedPermsInput,
} from "../Home/Post/GroupFeedPerms";
import { FormSubmit } from "~/views/components/FormSubmit";
import { StatelessAsyncToggle } from "~/views/components/StatelessAsyncToggle";

import useMetadataState from '~/logic/state/metadata';


interface FormSchema {
  permissions: GroupFeedPermissions;
}

export function GroupFeedSettings(props: {
  association: Association;
  group: Group;
  api: GlobalApi;
}) {
  const { association, group, api } = props;
  const resource = resourceFromPath(association.group);
  const feedResource = association?.metadata.config?.group?.resource;
  const isEnabled = !!feedResource;

  const associations = useMetadataState(state => state.associations);
  const feedMetadata = associations?.graph[feedResource];
  const vip = feedMetadata?.vip || '';

  const toggleFeed = async (actions: any) => {
    if (isEnabled) {
      await api.graph.disableGroupFeed(resource);
    } else {
      await api.graph.enableGroupFeed(resource, vip);
    }
  };
  const writers: Set<string> | undefined =
    group.tags.graph?.[feedResource]?.writers;
  const initialValues: FormSchema = {
    permissions: vip === '' && !writers ? 'everyone' :
      writers.size === 1 && writers.has(window.ship)
      ? "host"
      : "admins",
  };
  const onSubmit = async (
    values: FormSchema,
    actions: FormikHelpers<FormSchema>
  ) => {
    const tag = {
      app: "graph",
      resource: association.metadata.config?.group.resource,
      tag: "writers",
    };

    if (values.permissions === "admins") {
      const admins =
        Array.from(group.tags?.role?.admin).map((s) => `~${s}`) ?? [];

      await api.groups.addTag(resource, tag, admins);
    } else if (values.permissions === "host") {
      await api.groups.addTag(resource, tag, [`~${window.ship}`]);
    }
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
          {isEnabled && false && (
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
