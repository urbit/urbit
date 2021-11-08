import { Button, Col, Row, Text } from '@tlon/indigo-react';
import { createGroupFeed, Resource, resourceFromPath } from '@urbit/api';
import { Form, Formik, FormikHelpers } from 'formik';
import React from 'react';
import { useHistory } from 'react-router-dom';
import { AsyncButton } from '~/views/components/AsyncButton';
import { ModalOverlay } from '~/views/components/ModalOverlay';
import {
  GroupFeedPermsInput
} from './Post/GroupFeedPerms';
import airlock from '~/logic/api';

interface FormSchema {
  permissions: any;
}

export function EnableGroupFeed(props: {
  groupPath: string;
  dismiss?: () => void;
  baseUrl: string;
}) {
  const { groupPath, baseUrl } = props;

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
      await airlock.thread<Resource>(createGroupFeed(resource, values.permissions.trim()));
      actions.setStatus({ success: null });
      history.replace(`${baseUrl}/feed`);
    };

  return (
    <ModalOverlay spacing={[3, 5, 7]} bg="white" dismiss={dismiss}>
      <Formik initialValues={initialValues} onSubmit={onSubmit}>
        <Form>
          <Col gapY={3} p={3}>
            <Col gapY={1}>
              <Text fontWeight="medium" fontSize={2}>
                Enable Feed
              </Text>
              <Text gray>
                A central place to broadcast short posts with your group
              </Text>
            </Col>
            <GroupFeedPermsInput id="permissions" />
            <Row gapX={2}>
              <AsyncButton primary>Enable Feed</AsyncButton>
              <Button color="gray" onClick={dismiss}>Cancel</Button>
            </Row>
          </Col>
        </Form>
      </Formik>
    </ModalOverlay>
  );
}
