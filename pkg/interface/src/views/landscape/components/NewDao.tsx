import {
  Box, Button, Col,
  Icon,
  ManagedTextInputField as Input, Row, Text
} from '@tlon/indigo-react';
import { FieldArray, Form, Formik, FormikHelpers } from 'formik';
import React, { ReactElement, useCallback } from 'react';
import { useHistory } from 'react-router-dom';
import * as Yup from 'yup';
import { useWaitForProps } from '~/logic/lib/useWaitForProps';
import { stringToSymbol } from '~/logic/lib/util';
import useMetadataState from '~/logic/state/metadata';
import { AsyncButton } from '~/views/components/AsyncButton';
import airlock from '~/logic/api';
import useGroupState from '~/logic/state/group';

const formSchema = Yup.object({
  title: Yup.string()
    .matches(/^([a-zA-Z]|[\u2700-\u27bf]|(?:\ud83c[\udde6-\uddff]){2}|[\ud800-\udbff][\udc00-\udfff]|[\u0023-\u0039]\ufe0f?\u20e3|\u3299|\u3297|\u303d|\u3030|\u24c2|\ud83c[\udd70-\udd71]|\ud83c[\udd7e-\udd7f]|\ud83c\udd8e|\ud83c[\udd91-\udd9a]|\ud83c[\udde6-\uddff]|\ud83c[\ude01-\ude02]|\ud83c\ude1a|\ud83c\ude2f|\ud83c[\ude32-\ude3a]|\ud83c[\ude50-\ude51]|\u203c|\u2049|[\u25aa-\u25ab]|\u25b6|\u25c0|[\u25fb-\u25fe]|\u00a9|\u00ae|\u2122|\u2139|\ud83c\udc04|[\u2600-\u26FF]|\u2b05|\u2b06|\u2b07|\u2b1b|\u2b1c|\u2b50|\u2b55|\u231a|\u231b|\u2328|\u23cf|[\u23e9-\u23f3]|[\u23f8-\u23fa]|\ud83c\udccf|\u2934|\u2935|[\u2190-\u21ff]).*$/, 'Dao names must start with letters or emoji')
    .required('DAO must have a name'),
  description: Yup.string(),
  owners: Yup.array().of(
    Yup.string()
      .matches(/^0x[a-fA-F0-9]{40}$/)
      .required('Must be a valid address starting with \'0x\'')
  ).min(1),
  groupName: Yup.string()
    .matches(/^([a-zA-Z]|[\u2700-\u27bf]|(?:\ud83c[\udde6-\uddff]){2}|[\ud800-\udbff][\udc00-\udfff]|[\u0023-\u0039]\ufe0f?\u20e3|\u3299|\u3297|\u303d|\u3030|\u24c2|\ud83c[\udd70-\udd71]|\ud83c[\udd7e-\udd7f]|\ud83c\udd8e|\ud83c[\udd91-\udd9a]|\ud83c[\udde6-\uddff]|\ud83c[\ude01-\ude02]|\ud83c\ude1a|\ud83c\ude2f|\ud83c[\ude32-\ude3a]|\ud83c[\ude50-\ude51]|\u203c|\u2049|[\u25aa-\u25ab]|\u25b6|\u25c0|[\u25fb-\u25fe]|\u00a9|\u00ae|\u2122|\u2139|\ud83c\udc04|[\u2600-\u26FF]|\u2b05|\u2b06|\u2b07|\u2b1b|\u2b1c|\u2b50|\u2b55|\u231a|\u231b|\u2328|\u23cf|[\u23e9-\u23f3]|[\u23f8-\u23fa]|\ud83c\udccf|\u2934|\u2935|[\u2190-\u21ff]).*$/, 'Dao names must start with letters or emoji')
    .required('Initial group must have a name'),
  isPrivate: Yup.boolean()
});

interface FormSchema {
  title: string;
  description: string;
  owners: string[];
  groupName: string;
  isPrivate: boolean;
}

export function NewDao(): ReactElement {
  const history = useHistory();
  const initialValues: FormSchema = {
    title: '',
    description: '',
    owners: [''],
    groupName: '',
    isPrivate: false
  };

  // TODO: determine when DAO is created
  const groups = useGroupState(state => state.groups);
  const associations = useMetadataState(state => state.associations);
  const waiter = useWaitForProps({ groups, associations });

  const onSubmit = useCallback(
    async (values: FormSchema, actions: FormikHelpers<FormSchema>) => {
      try {
        const { title, description, isPrivate } = values;
        const name = stringToSymbol(title.trim());
        const policy: any = isPrivate
          ? {
              invite: {
                pending: []
              }
            }
          : {
              open: {
                banRanks: [],
                banned: []
              }
            };
        console.log('CREATE DAO:', name, policy, title, description)
        // create the DAO (fake it in memory)
        // wait for the DAO to exist
        // navigate to the DAO view

        // await airlock.thread(createDao(name, policy, title, description));
        // const path = `/ship/~${window.ship}/${name}`;
        // await waiter((p) => {
        //   return path in p.groups && path in p.associations.groups;
        // });

        // actions.setStatus({ success: null });
        // history.push(`/~landscape${path}`);
      } catch (e) {
        console.error(e);
        actions.setStatus({ error: e.message });
      }
    },
    [waiter, history]
  );

  return (
    <>
      <Col overflowY="auto" p={3}>
        <Box mb={3}>
          <Text fontWeight="bold">New DAO</Text>
        </Box>
        <Formik
          validationSchema={formSchema}
          initialValues={initialValues}
          onSubmit={onSubmit}
          render={({ values }) => (
          <Form>
            <Col gapY={4}>
              <Input
                id="title"
                label="Name"
                placeholder="eg. My DAO"
              />
              <Input
                id="description"
                label="Description"
                placeholder="DAO description"
              />
              <Input
                id="groupName"
                label="Initial group name"
                caption="The first group in your DAO"
                placeholder="eg. Main"
              />
              <FieldArray
                name="owners"
                render={arrayHelpers => (
                  <Box width="100%">
                    <Box mb={2}>
                      <Text>Owners</Text>
                    </Box>
                    {values.owners.map((owner, index) => (
                      <Row key={index} mb={2} width="100%">
                        <Input
                          id={`owners.${index}`}
                          placeholder="Address"
                          flex="auto"
                        />
                        {/* <Field name={`owners.${index}`} /> */}
                        <Icon p={2} pb="7px" ml={2} border="1px solid lightGray" borderRadius={2} icon="Minus" onClick={() => arrayHelpers.remove(index)} />
                        <Icon p={2} pb="7px" ml={2} border="1px solid lightGray" borderRadius={2} icon="Plus" onClick={() => arrayHelpers.insert(index, '')} />
                      </Row>
                    ))}
                    <Button type="button" onClick={() => arrayHelpers.push('')}>
                      Add an owner
                    </Button>
                  </Box>
                )}
              />
              {/* TODO: initializing the DAO should also have an input for voting threshold—how many owners need to sign off on a DAO action */}
              <AsyncButton>Create Dao</AsyncButton>
            </Col>
          </Form>
        )}
        />
      </Col>
    </>
  );
}
