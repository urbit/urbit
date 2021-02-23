import React from 'react';

import { Box } from '@tlon/indigo-react';

import { StoreState } from '~/logic/store/type';
import DisplayForm from './lib/DisplayForm';
import S3Form from './lib/S3Form';
import SecuritySettings from './lib/Security';
import RemoteContentForm from './lib/RemoteContent';

type ProfileProps = StoreState & { ship: string };

export default function Settings({
  s3
}: ProfileProps) {
  return (
    <Box
      backgroundColor="white"
      display="grid"
      gridTemplateRows="auto"
      gridTemplateColumns="1fr"
      gridRowGap={7}
      p={4}
      maxWidth="500px"
    >
      <DisplayForm
        s3={s3}
      />
      <RemoteContentForm />
      <S3Form s3={s3} />
      <SecuritySettings />
    </Box>
  );
}
