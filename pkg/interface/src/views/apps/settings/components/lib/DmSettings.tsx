import {
  Text,
  Col,
  Box,
  ManagedToggleSwitchField
} from '@tlon/indigo-react';
import { Form, Formik } from 'formik';
import React, { useCallback } from 'react';
import useGraphState from '~/logic/state/graph';
import { AsyncButton } from '~/views/components/AsyncButton';
import airlock from '~/logic/api';
import { setScreen } from '@urbit/api/graph';

export function DmSettings() {
  const screening = useGraphState(s => s.screening);
  const initialValues = { accept: !screening };
  const onSubmit = useCallback(
    async (values, actions) => {
      await airlock.poke(setScreen(!values.accept));
      actions.setStatus({ success: null });
    },
    [screening]
  );

  return (
    <Col p="5" gapY="5" width="100%" height="100%">
      <Col gapY="1">
        <Box>
          <Text fontSize="2" >Privacy</Text>
        </Box>
        <Box fontSize="1" color="gray">
          <Text gray>
            Control other people&apos;s ability to message you
          </Text>
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
