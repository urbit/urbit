import {
    Box,
    Col, ManagedTextInputField as Input,
    ManagedToggleSwitchField as Checkbox,
    Text
} from '@tlon/indigo-react';
import _ from 'lodash';
import { changePolicy, Enc } from '@urbit/api';
import { Group, GroupPolicy } from '@urbit/api/groups';
import { Association, metadataEdit, MetadataEditField } from '@urbit/api/metadata';
import { Form, Formik, FormikHelpers } from 'formik';
import React from 'react';
import * as Yup from 'yup';
import { resourceFromPath, roleForShip } from '~/logic/lib/group';
import { uxToHex } from '~/logic/lib/util';
import { AsyncButton } from '~/views/components/AsyncButton';
import { ColorInput } from '~/views/components/ColorInput';
import { FormError } from '~/views/components/FormError';
import { ImageInput } from '~/views/components/ImageInput';
import airlock from '~/logic/api';

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
}

export function GroupAdminSettings(props: GroupAdminSettingsProps) {
  const { group, association } = props;
  const { metadata } = association;
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
      const { color, isPrivate, adminMetadata } = values;
      const update = (upd: MetadataEditField) =>
        airlock.poke(metadataEdit(association, upd));

      const uxColor = uxToHex(color);
      const vip = adminMetadata ? '' : 'member-metadata';
      const promises = _.compact(_.map(['title', 'description', 'picture'] as const,
        (k) => {
          const edit: MetadataEditField = { [k]: values[k] };
          return (values[k] !== initialValues[k])
            ? update(edit)
            : null;
        }));
      if(vip !== metadata.vip) {
        promises.push(update({ vip }));
      }
      if(uxColor !== metadata.color) {
        promises.push(update({ color: uxColor }));
      }
      await Promise.all(promises);
      if (isPrivate !== currentPrivate) {
        const resource = resourceFromPath(props.association.group);
        const newPolicy: Enc<GroupPolicy> = isPrivate
          ? { invite: { pending: [] } }
          : { open: { banRanks: [], banned: [] } };
        const diff = { replace: newPolicy };
        await airlock.poke(changePolicy(resource, diff));
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
        <Box p={4} id="group-details"><Text fontWeight="600" fontSize={2}>Group Details</Text></Box>
        <Col pb={4} px={4} maxWidth="384px" gapY={4}>
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
