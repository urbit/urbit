import React from 'react';
import { Meta } from '@storybook/react';

import { Box } from '@tlon/indigo-react';
import { PendingDm } from '~/views/apps/notifications/PendingDm';

export default {
  title: 'Notifications/PendingDm',
  component: PendingDm
} as Meta;

export const Default = () => (
  <Box width="95%" p="1" backgroundColor="white">
    <PendingDm  ship="~hastuc-dibtux" />
  </Box>
);
