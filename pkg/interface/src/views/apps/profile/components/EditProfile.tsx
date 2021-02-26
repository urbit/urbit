import React, { ReactElement } from 'react';
import * as Yup from 'yup';
import _ from 'lodash';
import { Formik } from 'formik';
import { useHistory } from 'react-router-dom';

import {
  ManagedForm as Form,
  ManagedTextInputField as Input,
  ManagedCheckboxField as Checkbox,
  Col,
  Text,
  Row,
} from '@tlon/indigo-react';

import { uxToHex } from '~/logic/lib/util';
import { AsyncButton } from '~/views/components/AsyncButton';
import { ColorInput } from '~/views/components/ColorInput';
import { ImageInput } from '~/views/components/ImageInput';
import { MarkdownField } from '~/views/apps/publish/components/MarkdownField';
import { resourceFromPath } from '~/logic/lib/group';
import GroupSearch from '~/views/components/GroupSearch';
import useContactState from '~/logic/state/contacts';

const formSchema = Yup.object({
  nickname: Yup.string(),
  bio: Yup.string(),
  color: Yup.string(),
  avatar: Yup.string().nullable()
});

const emptyContact = {
  nickname: '',
  bio: '',
  status: '',
  color: '0',
  avatar: null,
  cover: null,
  groups: [],
  'last-updated': 0,
  isPublic: false
};

export function EditProfile(props: any): ReactElement {
  const { contact, ship, api } = props;
  const isPublic = useContactState(state => state.isContactPublic);
  const history = useHistory();
  if (contact) {
    contact.isPublic = isPublic;
  }

  const onSubmit = async (values: any, actions: any) => {
    console.log(values);
    try {
      await Object.keys(values).reduce((acc, key) => {
        console.log(key);
        const newValue = key !== 'color' ? values[key] : uxToHex(values[key]);

        if (newValue !== contact[key]) {
          if (key === 'isPublic') {
            return acc.then(() =>
              api.contacts.setPublic(newValue)
            );
          } else if (key === 'groups') {
            const toRemove: string[] = _.difference(contact?.groups || [], newValue);
            console.log(toRemove);
            const toAdd: string[] = _.difference(newValue, contact?.groups || []);
            console.log(toAdd);
            const promises: Promise<any>[] = [];

            promises.concat(
              toRemove.map(e =>
                api.contacts.edit(ship, { 'remove-group': resourceFromPath(e) })
              )
            );
            promises.concat(
              toAdd.map(e =>
                api.contacts.edit(ship, { 'add-group': resourceFromPath(e) })
              )
            );
            return acc.then(() => Promise.all(promises));
          } else if (
            key !== 'last-updated' &&
            key !== 'isPublic'
          ) {
            return acc.then(() =>
              api.contacts.edit(ship, { [key]: newValue })
            );
          }
        }
        return acc;
      }, Promise.resolve());
      // actions.setStatus({ success: null });
      history.push(`/~profile/${ship}`);
    } catch (e) {
      console.error(e);
      actions.setStatus({ error: e.message });
    }
  };

  return (
    <>
      <Formik
        validationSchema={formSchema}
        initialValues={contact || emptyContact}
        onSubmit={onSubmit}
      >
      <Form width="100%" height="100%" p={2}>
        <Input id="nickname" label="Name" mb={3} />
        <Col width="100%">
          <Text mb={2}>Description</Text>
          <MarkdownField id="bio" mb={3} s3={props.s3} />
        </Col>
        <ColorInput id="color" label="Sigil Color" mb={3} />
        <Row mb={3} width="100%">
          <Col pr={2} width="50%">
            <ImageInput id="cover" label="Cover Image" s3={props.s3} />
          </Col>
          <Col pl={2} width="50%">
            <ImageInput id="avatar" label="Profile Image" s3={props.s3} />
          </Col>
        </Row>
        <Checkbox mb={3} id="isPublic" label="Public Profile" />
        <GroupSearch label="Pinned Groups" id="groups" publicOnly />
        <AsyncButton primary loadingText="Updating..." border mt={3}>
          Submit
        </AsyncButton>
      </Form>
    </Formik>
  </>
  );
}
