import React from 'react';
import { Story, Meta } from '@storybook/react';

import { Box } from '@tlon/indigo-react';
import { Formik } from 'formik';
import { ColorInput, ColorInputProps } from '~/views/components/ColorInput';

const initialValues = {
  color: '33FF22'
};

export default {
  title: 'Form/ColorInput',
  component: ColorInput
} as Meta;

const Template: Story<ColorInputProps> = args => (
  <Box backgroundColor="white" p="2" width="fit-content">
    <Formik initialValues={initialValues} onSubmit={() => {}}>
      <ColorInput {...args} id="color" />
    </Formik>
  </Box>
);

export const Label = Template.bind({});

Label.args = {
  label: 'A color input',
  placeholder: '#444444'
};

export const NoLabel = Template.bind({});

NoLabel.args = {
  placeholder: '#444444'
};

export const Disabled = Template.bind({});

Disabled.args = {
  disabled: true
};
