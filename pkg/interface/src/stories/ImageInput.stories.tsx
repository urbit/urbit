import React from 'react';
import { Story, Meta } from '@storybook/react';

import { Box } from '@tlon/indigo-react';
import { Formik } from 'formik';
import { ImageInput, ImageInputProps } from '~/views/components/ImageInput';

const initialValues = {
  url: undefined
};

export default {
  title: 'Form/ImageInput',
  component: ImageInput
} as Meta;

const Template: Story<ImageInputProps> = args => (
  <Box backgroundColor="white" p="2">
    <Formik initialValues={initialValues} onSubmit={() => {}}>
      <ImageInput {...args} id="url" />
    </Formik>
  </Box>
);

export const Label = Template.bind({});

Label.args = {
  id: 'url',
  label: 'An image upload',
  placeholder: 'http://image.hoster'
};
