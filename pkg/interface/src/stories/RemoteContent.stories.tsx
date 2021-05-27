import React from 'react';
import { Story, Meta } from '@storybook/react';

import { Box } from '@tlon/indigo-react';
import RemoteContent, {
  RemoteContentProps
} from '~/views/components/RemoteContent';

export default {
  title: 'Content/RemoteContent',
  component: RemoteContent
} as Meta;

const Template: Story<RemoteContentProps> = args => (
  <Box backgroundColor="white" p="2" width="500px">
    <RemoteContent {...args} />
  </Box>
);

export const Youtube = Template.bind({});

Youtube.args = {
  unfold: true,
  url: 'https://www.youtube.com/watch?v=M04AKTCDavc&t=1s'
};

export const Video = Template.bind({});

Video.args = {
  url: 'https://media.urbit.org/site/sea30-1440.mp4',
  unfold: true
};

export const Twitter = Template.bind({});

Twitter.args = {
  url: 'https://twitter.com/urbit/status/1396947489656213504',
  // massive test flake
  unfold: false
};
