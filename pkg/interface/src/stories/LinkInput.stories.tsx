import React from 'react';
import { Story, Meta } from '@storybook/react';
import { withDesign } from 'storybook-addon-designs';

import { Box } from '@tlon/indigo-react';
import {
  LinkInput,
  LinkInputProps
} from '~/views/apps/links/components/LinkInput';

export default {
  title: 'Input/Collections',
  component: LinkInput,
  decorators: [withDesign]
} as Meta;

const Template: Story<LinkInputProps> = args => (
  <Box p="1" width="500px" backgroundColor="white">
    <LinkInput {...args} />
  </Box>
);

export const Empty = Template.bind({});

Empty.parameters = {
  design: {
    type: 'figma',
    url:
      'https://www.figma.com/file/H1RAHV4KscSTnvrIiL0z8C/Indigo?node-id=5113%3A60'
  }
};

export const Filled = Template.bind({});

Filled.parameters = {
  design: {
    type: 'figma',
    url:
      'https://www.figma.com/file/H1RAHV4KscSTnvrIiL0z8C/Indigo?node-id=5113%3A82'
  }
};

Filled.args = {
  url: 'https://www.youtube.com/watch?v=a_EvjlM7We0',
  title: 'Gigi Masin - Stella Maris'
};

export const Overflow = Template.bind({});

Overflow.args = {
  url:
    'https://www.youtube.com/watch?v=a_EvjlM7We0https://www.youtube.com/watch?v=a_EvjlM7We0',
  title: 'Gigi Masin - Stella Maris'
};

Overflow.parameters = {
  design: {
    type: 'figma',
    url:
      'https://www.figma.com/file/H1RAHV4KscSTnvrIiL0z8C/Indigo?node-id=5113%3A97'
  }
};
