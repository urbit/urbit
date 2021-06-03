import React from 'react';
import { Meta } from '@storybook/react';

import { Box } from '@tlon/indigo-react';
import { PendingDm } from '~/views/apps/notifications/PendingDm';

export default {
  title: 'Notifications/PendingDm',
  component: PendingDm
} as Meta;
const fakeApi = {} as any;

export const Default = () => (
  <Box width="95%" p="1" backgroundColor="white">
    <PendingDm api={fakeApi} ship="~hastuc-dibtux" />
  </Box>
);
