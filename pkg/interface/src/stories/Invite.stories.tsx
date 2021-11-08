import React from 'react';
import { Meta, Story } from '@storybook/react';

import { Box } from '@tlon/indigo-react';
import { InviteItem, InviteItemProps } from '~/views/components/Invite';
import { JoinProgress } from '@urbit/api/groups';

export default {
  title: 'Notifications/Invite',
  component: InviteItem
} as Meta;

const Template: Story<InviteItemProps> = args => (
  <Box backgroundColor="white" p="0" maxWidth="90%" width="fit-content">
    <InviteItem {...args} />
  </Box>
);

const pendingJoin = (progress: JoinProgress) => ({
  hidden: false,
  started: Date.now() - 3600,
  ship: '~haddef-sigwen',
  progress
});

export const Pending = Template.bind({});
Pending.args = {
  pendingJoin: pendingJoin('start'),
  resource: '/ship/~bollug-worlus/urbit-index'
};

export const Errored = Template.bind({});
Errored.args = {
  pendingJoin: pendingJoin('no-perms'),
  resource: '/ship/~bollug-worlus/urbit-index'
};

export const Done = Template.bind({});
Done.args = {
  pendingJoin: pendingJoin('done'),
  resource: '/ship/~bollug-worlus/urbit-index'
};
