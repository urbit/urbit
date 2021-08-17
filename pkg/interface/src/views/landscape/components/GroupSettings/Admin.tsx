import React from 'react';
import { Formik, Form, FormikHelpers } from 'formik';
import * as Yup from 'yup';

import {
  Box,
  ManagedTextInputField as Input,
  ManagedToggleSwitchField as Checkbox,
  Col,
  Text
} from '@tlon/indigo-react';
import { Enc } from '@urbit/api';
import { Group, GroupPolicy } from '@urbit/api/groups';
import { Association } from '@urbit/api/metadata';

import { AsyncButton } from '~/views/components/AsyncButton';
import { FormError } from '~/views/components/FormError';
import GlobalApi from '~/logic/api/global';
import { resourceFromPath, roleForShip } from '~/logic/lib/group';
import { ColorInput } from '~/views/components/ColorInput';
import { useHistory } from 'react-router-dom';
import { uxToHex } from '~/logic/lib/util';
import { ImageInput } from '~/views/components/ImageInput';
import { StorageState } from '~/types';

interface FormSchema {
  title: string;
  description: string;
  color: string;
  isPrivate: boolean;
  picture: string;
  adminMetadata: boolean;
}

const formSchema = Yup.object({
  title: Yup.string().required('Group must have a name'),
  description: Yup.string(),
  color: Yup.string(),
  isPrivate: Yup.boolean(),
  adminMetadata: Yup.boolean()
});

interface GroupAdminSettingsProps {
  group: Group;
  association: Association;
  api: GlobalApi;
}

export function GroupAdminSettings(props: GroupAdminSettingsProps) {
  const { group, association } = props;
  const { metadata } = association;
  const history = useHistory();
  const currentPrivate = 'invite' in props.group.policy;
  const initialValues: FormSchema = {
    title: metadata?.title,
    description: metadata?.description,
    color: metadata?.color,
    picture: metadata?.picture,
    isPrivate: currentPrivate,
    adminMetadata: metadata.vip !== 'member-metadata'
  };

  const onSubmit = async (
    values: FormSchema,
    actions: FormikHelpers<FormSchema>
  ) => {
    try {
      const { title, description, picture, color, isPrivate, adminMetadata } = values;
      const uxColor = uxToHex(color);
      const vip = adminMetadata ? '' : 'member-metadata';
      await props.api.metadata.update(props.association, {
        title,
        description,
        picture,
        color: uxColor,
        vip
      });
      if (isPrivate !== currentPrivate) {
        const resource = resourceFromPath(props.association.group);
        const newPolicy: Enc<GroupPolicy> = isPrivate
          ? { invite: { pending: [] } }
          : { open: { banRanks: [], banned: [] } };
        const diff = { replace: newPolicy };
        await props.api.groups.changePolicy(resource, diff);
      }

      actions.setStatus({ success: null });
    } catch (e) {
      console.log(e);
      actions.setStatus({ error: e.message });
    }
  };

  const disabled =
    resourceFromPath(association.group).ship.slice(1) !== window.ship &&
    roleForShip(group, window.ship) !== 'admin';
  if(disabled)
return null;

  return (
    <Formik
      validationSchema={formSchema}
      initialValues={initialValues}
      onSubmit={onSubmit}
    >
      <Form>
        <Box p="4" id="group-details"><Text fontWeight="600" fontSize="2">Group Details</Text></Box>
        <Col pb="4" px="4" maxWidth="384px" gapY={4}>
          <Input
            id="title"
            label="Group Name"
            caption="The name for your group to be called by"
            disabled={disabled}
          />
          <Input
            id="description"
            label="Group Description"
            caption="The description of your group"
            disabled={disabled}
          />
          <ColorInput
            id="color"
            label="Group color"
            caption="A color to represent your group"
            disabled={disabled}
          />
          <ImageInput
            id="picture"
            label="Group picture"
            caption="A picture for your group"
            placeholder="Enter URL"
            disabled={disabled}
          />
          <Checkbox
            id="isPrivate"
            label="Private group"
            caption="If enabled, users must be invited to join the group"
            disabled={disabled}
          />
          <Checkbox
            id="adminMetadata"
            label="Restrict channel adding to admins"
            caption="If enabled, users must be an admin to add a channel to the group"
            disabled={disabled}
          />

          <AsyncButton
            disabled={disabled}
            primary
            loadingText="Updating.."
            border
          >
            Save
          </AsyncButton>
          <FormError message="Failed to update settings" />
        </Col>
      </Form>
    </Formik>
  );
}
