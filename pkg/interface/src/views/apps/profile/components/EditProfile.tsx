import {
    Button, Col, ManagedCheckboxField as Checkbox, ManagedForm as Form,
    ManagedTextInputField as Input,

    Row, Text
} from '@tlon/indigo-react';
import { Formik } from 'formik';
import _ from 'lodash';
import React, { ReactElement, useState } from 'react';
import { useHistory } from 'react-router-dom';
import * as Yup from 'yup';
import { resourceFromPath } from '~/logic/lib/group';
import { uxToHex } from '~/logic/lib/util';
import useContactState from '~/logic/state/contact';
import { MarkdownField } from '~/views/apps/publish/components/MarkdownField';
import { AsyncButton } from '~/views/components/AsyncButton';
import { ColorInput } from '~/views/components/ColorInput';
import GroupSearch from '~/views/components/GroupSearch';
import { ImageInput } from '~/views/components/ImageInput';
import {
    ProfileControls, ProfileHeader,

    ProfileImages, ProfileStatus
} from './Profile';
import airlock from '~/logic/api';
import { editContact, setPublic } from '@urbit/api';
import { TextLink } from '~/views/components/Link';

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
  avatar: '',
  cover: '',
  groups: [],
  'last-updated': 0,
  isPublic: false
};

export function ProfileHeaderImageEdit(props: any): ReactElement {
  const { contact, setFieldValue, handleHideCover } = props;
  const [editCover, setEditCover] = useState(false);
  const [removedCoverLabel, setRemovedCoverLabel] = useState('Remove Header');
  const handleClear = (e) => {
    e.preventDefault();
    handleHideCover(true);
    setFieldValue('cover', '');
    setRemovedCoverLabel('Header Removed');
  };

  return (
    <>
      {contact?.cover ? (
        <div>
          {editCover ? (
            <ImageInput id='cover' marginTop='-8px' width='288px' />
          ) : (
            <Row>
              <Button mr={2} onClick={() => setEditCover(true)}>
                Replace Header
              </Button>
              <Button onClick={e => handleClear(e)}>
                {removedCoverLabel}
              </Button>
            </Row>
          )}
        </div>
      ) : (
        <ImageInput id='cover' marginTop='-8px' width='288px' />
      )}
    </>
  );
}

export function EditProfile(props: any): ReactElement {
  const { contact, ship } = props;
  const isPublic = useContactState(state => state.isContactPublic);
  const [hideCover, setHideCover] = useState(false);

  const handleHideCover = (value) => {
    setHideCover(value);
  };

  const history = useHistory();

  const onSubmit = async (values: any, actions: any) => {
    try {
      Object.keys(values).forEach((key) => {
        const newValue = key !== 'color' ? values[key] : uxToHex(values[key]);
        if (newValue !== contact[key]) {
          if (key === 'isPublic') {
            airlock.poke(setPublic(true));
            return;
          } else if (key === 'groups') {
            const toRemove: string[] = _.difference(
              contact?.groups || [],
              newValue
            );
            const toAdd: string[] = _.difference(
              newValue,
              contact?.groups || []
            );
            toRemove.forEach(e =>
                airlock.poke(editContact(ship, { 'remove-group': resourceFromPath(e) }))
            );
              toAdd.forEach(e =>
                airlock.poke(editContact(ship, { 'add-group': resourceFromPath(e) }))
            );
          } else if (key !== 'last-updated' && key !== 'isPublic') {
            airlock.poke(editContact(ship, { [key]: newValue }));
            return;
          }
        }
      });
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
        initialValues={{ ...contact, isPublic } || emptyContact}
        onSubmit={onSubmit}
      >
        {({ setFieldValue }) => (
          <Form width='100%' height='100%'>
            <ProfileHeader>
              <ProfileControls>
                <Row alignItems='baseline'>
                  <Button
                    type='submit'
                    display='inline'
                    cursor='pointer'
                    fontWeight='500'
                    color='blue'
                    pl={0}
                    pr={0}
                    border={0}
                    style={{ appearance: 'none', background: 'transparent' }}
                  >
                    Save Edits
                  </Button>
                  <TextLink
                    py={2}
                    ml={3}
                    fontWeight='500'
                    to={`/~profile/${ship}`}
                  >
                    Cancel
                  </TextLink>
                </Row>
                <ProfileStatus contact={contact} />
              </ProfileControls>
              <ProfileImages
                hideCover={hideCover}
                contact={contact}
                ship={ship}
              >
                <ProfileHeaderImageEdit
                  contact={contact}
                  setFieldValue={setFieldValue}
                  handleHideCover={handleHideCover}
                />
              </ProfileImages>
            </ProfileHeader>
            <Row mb={3} pt={5} width='100%'>
              <Col pr={2} width='25%'>
                <ColorInput id='color' label='Sigil Color' />
              </Col>
              <Col pl={2} width='75%'>
                <ImageInput
                  id='avatar'
                  label='Overlay Avatar (may be hidden by other users)'
                />
              </Col>
            </Row>
            <Input id='nickname' label='Custom Name' mb={3} />
            <Col width='100%'>
              <Text mb={2}>Description</Text>
              <MarkdownField id='bio' mb={3} />
            </Col>
            <Checkbox mb={3} id='isPublic' label='Public Profile' />
            <GroupSearch label='Pinned Groups' id='groups' publicOnly />
            <AsyncButton primary loadingText='Updating...' border mt={3}>
              Submit
            </AsyncButton>
          </Form>
        )}
      </Formik>
    </>
  );
}
