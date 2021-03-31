import React, { useCallback } from "react";
import { ModalOverlay } from "~/views/components/ModalOverlay";
import { Formik, Form, FormikHelpers } from "formik";
import {
  GroupFeedPermissions,
  GroupFeedPermsInput,
} from "./Post/GroupFeedPerms";
import { Text, Button, Col, Row } from "@tlon/indigo-react";
import { AsyncButton } from "~/views/components/AsyncButton";
import GlobalApi from "~/logic/api/global";
import { resourceFromPath, Tag, resourceAsPath } from "@urbit/api";
import useGroupState, { useGroup } from "~/logic/state/group";
import { useHistory } from 'react-router-dom';


interface FormSchema {
  permissions: GroupFeedPermissions;
}

export function EnableGroupFeed(props: {
  groupPath: string;
  dismiss: () => void;
  api: GlobalApi;
}) {
  const { api, groupPath, baseUrl } = props;

  const history = useHistory();
  const dismiss = () => {
    history.push(baseUrl);
  };

  const initialValues: FormSchema = {
    permissions: "everyone",
  };
  const group = useGroup(groupPath);
  const onSubmit = useCallback(
    async (values: FormSchema, actions: FormikHelpers<FormSchema>) => {
      const resource = resourceFromPath(groupPath);
      const feed = resourceAsPath(await api.graph.enableGroupFeed(resource));
      const tag: Tag = {
        app: "graph",
        resource: feed,
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
      dismiss();
    },
    [groupPath, baseUrl]
  );

  return (
    <ModalOverlay spacing={[3, 5, 7]} bg="white" dismiss={dismiss}>
      <Formik initialValues={initialValues} onSubmit={onSubmit}>
        <Form>
          <Col gapY="3" p="3">
            <Col gapY="1">
              <Text fontWeight="medium" fontSize="2">
                Enable Feed
              </Text>
              <Text gray>
                A central place to broadcast short posts with your group
              </Text>
            </Col>
            <GroupFeedPermsInput id="permissions" />
            <Row gapX="2">
              <AsyncButton primary>Enable Feed</AsyncButton>
              <Button gray onClick={dismiss}>Cancel</Button>
            </Row>
          </Col>
        </Form>
      </Formik>
    </ModalOverlay>
  );
}
