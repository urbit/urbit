import React from 'react';
import { Box, Col, Text } from '@tlon/indigo-react';
import * as Yup from 'yup';
import { Formik, FormikHelpers, Form } from 'formik';
import { useHistory } from 'react-router-dom';

import { Groups, Associations, Association } from '@urbit/api';

import GlobalApi from '~/logic/api/global';
import GroupSearch from '~/views/components/GroupSearch';
import { AsyncButton } from '~/views/components/AsyncButton';
import useGroupState from '~/logic/state/group';

const formSchema = Yup.object({
  group: Yup.string().nullable()
});

interface FormSchema {
  group: string[] | null;
}

interface GroupifyFormProps {
  api: GlobalApi;
  association: Association;
}

export function GroupifyForm(props: GroupifyFormProps) {
  const history = useHistory();
  const { association } = props;
  const onGroupify = async (
    values: FormSchema,
    actions: FormikHelpers<FormSchema>
  ) => {
    try {
      const rid = association.resource;
      const [, , ship, name] = rid;
      await props.api.graph.groupifyGraph(
        ship,
        name,
        values.group?.toString() || undefined
      );
      const mod = association.metadata.module || association['app-name'];
      const newGroup = values.group || association.group;
      history.push(`/~landscape${newGroup}/resource/${mod}${rid}`);
      actions.setStatus({ success: null });
    } catch (e) {
      console.error(e);
      actions.setStatus({ error: e.message });
    }
  };

  const groupPath = props.association?.group;
  const groups = useGroupState(state => state.groups);

  const isUnmanaged = groups?.[groupPath]?.hidden || false;

  if (!isUnmanaged) {
    return null;
  }

  const initialValues: FormSchema = {
    group: null
  };

  return (
    <Formik
      validationSchema={formSchema}
      initialValues={initialValues}
      onSubmit={onGroupify}
    >
      <Form>
        <Col flexShrink="0" gapY="4" maxWidth="512px">
          <Box>
            <Text fontWeight="500">Groupify this channel</Text>
          </Box>
          <GroupSearch
            id="group"
            label="Group"
            caption="Optionally, if you have admin privileges, you can add this channel to a group, or leave this blank to place the channel in its own group"
            adminOnly
            maxLength={1}
          />
          <AsyncButton primary loadingText="Groupifying..." border>
            Groupify
          </AsyncButton>
        </Col>
      </Form>
    </Formik>
  );
}

export default GroupifyForm;
