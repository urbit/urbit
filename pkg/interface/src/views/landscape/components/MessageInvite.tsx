import { Box, Col, Text } from '@tlon/indigo-react';
import { invite } from '@urbit/api/groups';
import { Form, Formik } from 'formik';
import _ from 'lodash';
import React from 'react';
import * as Yup from 'yup';
import { resourceFromPath } from '~/logic/lib/group';
import { deSig } from '~/logic/lib/util';
import { AsyncButton } from '~/views/components/AsyncButton';
import { ShipSearch } from '~/views/components/ShipSearch';
import airlock from '~/logic/api';

interface FormSchema {
  ships: string[];
}

const formSchema = Yup.object<FormSchema>({
  ships: Yup.array(Yup.string()).min(1, 'Must invite at least one ship')
});

export const MessageInvite = (props) => {
  const { association } = props;
  const initialValues: FormSchema = { ships: [] };
  const onSubmit = async ({ ships }: FormSchema, actions) => {
    try {
      const { ship, name } = resourceFromPath(association.group);
      await airlock.thread(invite(
        ship,
        name,
        _.compact(ships).map(s => `~${deSig(s)}`),
        `Inviting you to a DM with ~${ship}`
      ));
      actions.setStatus({ success: null });
    } catch (e) {
      console.error(e);
      actions.setStatus({ error: e.message });
    }
  };
  return (
    <Col p={3}>
      <Formik
        initialValues={initialValues}
        onSubmit={onSubmit}
        validationSchema={formSchema}
        validateOnBlur
      >
        <Form>
          <Box mb={3}>
            <Text fontSize={2} bold>Add to Group Message</Text>
          </Box>
          <ShipSearch id='ships' label='Invitees' autoFocus />
          <AsyncButton border={0} primary loadingText='Inviting...'>
            Invite
          </AsyncButton>
        </Form>
      </Formik>
    </Col>
  );
};

export default MessageInvite;
