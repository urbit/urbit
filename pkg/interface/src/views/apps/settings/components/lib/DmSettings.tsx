import {
  Text,
  Col,
  Box,
  ManagedToggleSwitchField
} from '@tlon/indigo-react';
import { Form } from 'formik';
import React, { useCallback } from 'react';
import useGraphState, { GraphState } from '~/logic/state/graph';
import { FormikOnBlur } from '~/views/components/FormikOnBlur';
import shallow from 'zustand/shallow';

const selInit = (s: GraphState) => ({
  accept: !s.screening
});

export function DmSettings() {
  const initialValues = useGraphState(selInit, shallow);
  const onSubmit = useCallback(
    async (values, actions) => {
      const { setScreen } = useGraphState.getState();
      setScreen(!values.accept);
      actions.setStatus({ success: null });
    },
    []
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
        <FormikOnBlur initialValues={initialValues} onSubmit={onSubmit}>
          <Form>
            <Col gapY="4">
              <ManagedToggleSwitchField
                id="accept"
                label="Auto-accept DM invites"
                caption="Direct messages will be automatically joined, and you wil see any messages sent in notifications"
              />
            </Col>
          </Form>
        </FormikOnBlur>
      </Col>
    </Col>
  );
}
