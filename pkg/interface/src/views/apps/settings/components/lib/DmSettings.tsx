import {
  Col,
  Box,
  ManagedToggleSwitchField
} from '@tlon/indigo-react';
import { Form, Formik } from 'formik';
import React, { useCallback } from 'react';
import GlobalApi from '~/logic/api/global';
import useGraphState from '~/logic/state/graph';
import { AsyncButton } from '~/views/components/AsyncButton';

export function DmSettings(props: { api: GlobalApi }) {
  const { api } = props;
  const screening = useGraphState(s => s.screening);
  const initialValues = { accept: !screening };
  const onSubmit = useCallback(
    async (values, actions) => {
      await api.graph.setScreen(!values.accept);
      actions.setStatus({ success: null });
    },
    [screening]
  );

  return (
    <Col p="5" gapY="5" width="100%" height="100%">
      <Col gapY="1">
        <Box color="black" fontSize="2">Privacy</Box>
        <Box fontSize="1" color="gray">
          Control other people&apos;s ability to message you
        </Box>
      </Col>
      <Col>
        <Box mb="4" fontWeight="medium" fontSize="1" color="gray">
          Direct Messages
        </Box>
        <Formik initialValues={initialValues} onSubmit={onSubmit}>
          <Form>
            <Col gapY="4">
              <ManagedToggleSwitchField
                id="accept"
                label="Auto-accept DM invites"
                caption="Direct messages will be automatically joined, and you wil see any messages sent in notifications"
              />
              <AsyncButton width="fit-content" primary>
                Save Changes
              </AsyncButton>
            </Col>
          </Form>
        </Formik>
      </Col>
    </Col>
  );
}
