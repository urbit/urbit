import React, { useCallback } from 'react';
import { ModalOverlay } from '~/views/components/ModalOverlay';
import { Formik, Form, FormikHelpers } from 'formik';
import {
  GroupFeedPermsInput
} from './Post/GroupFeedPerms';
import { Text, Button, Col, Row } from '@tlon/indigo-react';
import { AsyncButton } from '~/views/components/AsyncButton';
import GlobalApi from '~/logic/api/global';
import { resourceFromPath, Tag, resourceAsPath } from '@urbit/api';
import { useHistory } from 'react-router-dom';

interface FormSchema {
  permissions: any;
}

export function EnableGroupFeed(props: {
  groupPath: string;
  dismiss: () => void;
  api: GlobalApi;
  baseUrl: string;
}) {
  const { api, groupPath, baseUrl } = props;

  const history = useHistory();
  const dismiss = () => {
    history.replace(baseUrl);
  };

  const initialValues: FormSchema = {
    permissions: 'everyone'
  };
  const onSubmit =
    async (values: FormSchema, actions: FormikHelpers<FormSchema>) => {
      const resource = resourceFromPath(groupPath);
      const feed = resourceAsPath(
        await api.graph.enableGroupFeed(resource, values.permissions.trim())
      );
      actions.setStatus({ success: null });
      history.replace(`${baseUrl}/feed`);
    };

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
              <Button color="gray" onClick={dismiss}>Cancel</Button>
            </Row>
          </Col>
        </Form>
      </Formik>
    </ModalOverlay>
  );
}
