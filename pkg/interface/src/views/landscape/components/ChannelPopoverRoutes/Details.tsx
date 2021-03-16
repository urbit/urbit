import React from 'react';
import { Formik, Form } from 'formik';

import {
  ManagedTextInputField as Input,
  Col,
  Label,
  Text
} from '@tlon/indigo-react';
import { Association, metadata as metadataApi } from '@urbit/api';

import { FormError } from '~/views/components/FormError';
import { ColorInput } from '~/views/components/ColorInput';
import { uxToHex } from '~/logic/lib/util';
import { FormSubmit } from '~/views/components/FormSubmit';
import useApi from '~/logic/api';
import { update } from '@urbit/api/metadata';

interface FormSchema {
  title: string;
  description: string;
  color: string;
}

interface ChannelDetailsProps {
  association: Association;
}

export function ChannelDetails(props: ChannelDetailsProps) {
  const { association } = props;
  const { metadata } = association;
  const api = useApi();
  const initialValues: FormSchema = {
    title: metadata?.title || '',
    description: metadata?.description || '',
    color: metadata?.color || '0x0'
  };

  const onSubmit = async (values: FormSchema, actions) => {
    const { title, description } = values;
    const color = uxToHex(values.color);
    await api.poke(metadataApi.update(association, { title, color, description }));
    actions.setStatus({ success: null });
  };

  return (
    <Formik initialValues={initialValues} onSubmit={onSubmit}>
      <Form style={{ display: 'contents' }}>
        <Col mb="4" flexShrink={0} gapY="4">
          <Col mb={3}>
            <Text id="details" fontSize="2" fontWeight="bold">
              Channel Details
            </Text>
            <Label gray mt="2">
              Set the title, description and colour of the channel
            </Label>
          </Col>
          <Input
            id="title"
            label="Title"
            caption="Change the title of this channel"
          />
          <Input
            id="description"
            label="Change description"
            caption="Change the description of this channel"
          />
          <ColorInput
            id="color"
            label="Color"
            caption="Change the color of this channel"
          />
          <FormSubmit>
            Update Details
          </FormSubmit>
          <FormError message="Failed to update settings" />
        </Col>
      </Form>
    </Formik>
  );
}
