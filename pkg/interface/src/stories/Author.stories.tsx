import React from 'react';
import { Story, Meta } from '@storybook/react';

import { Box } from '@tlon/indigo-react';
import Author, { AuthorProps } from '~/views/components/Author';

export default {
  title: 'Identity/Author',
  component: Author
} as Meta;

const date = 1622093233566;

const Template: Story<AuthorProps> = args => (
  <Box backgroundColor="white" p="2" width="fit-content">
    <Author {...args} />
  </Box>
);

export const WithNicknameTime = Template.bind({});

WithNicknameTime.args = {
  ship: 'sampel-palnet',
  showImage: true,
  size: 24,
  sigilPadding: 6,
  date
};

export const WithNickname = Template.bind({});

WithNickname.args = {
  ship: 'sampel-palnet',
  showImage: true,
  size: 24,
  sigilPadding: 6,
  dontShowTime: true
};

export const NoContactTime = Template.bind({});

NoContactTime.args = {
  ship: 'sampel-sampel',
  showImage: true,
  size: 24,
  sigilPadding: 6,
  date
};

export const NoContact = Template.bind({});

NoContact.args = {
  ship: 'sampel-sampel',
  showImage: true,
  size: 24,
  sigilPadding: 6,
  dontShowTime: true
};

export const RelativeTime = Template.bind({});

RelativeTime.args = {
  ship: 'sampel-palnet',
  showImage: true,
  size: 24,
  sigilPadding: 6,
  isRelativeTime: true,
  date: Date.now() - 3600000
};
