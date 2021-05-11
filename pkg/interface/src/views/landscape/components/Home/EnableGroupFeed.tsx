import { Button, Col, Row, Text } from '@tlon/indigo-react';
import { resourceAsPath, resourceFromPath } from '@urbit/api';
import { Form, Formik, FormikHelpers } from 'formik';
import React from 'react';
import { useHistory } from 'react-router-dom';
import GlobalApi from '~/logic/api/global';
import { AsyncButton } from '~/views/components/AsyncButton';
import { ModalOverlay } from '~/views/components/ModalOverlay';
import {
  GroupFeedPermsInput
} from './Post/GroupFeedPerms';

interface FormSchema {
  permissions: any;
}

export function EnableGroupFeed(props: {
  groupPath: string;
  dismiss?: () => void;
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
        <Form className='enable-group-feed'>
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
