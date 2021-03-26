import React, { ReactElement } from 'react';
import { default as GenericBackButton } from '~/views/components/BackButton';

export function BackButton(): ReactElement {
  return (
    <GenericBackButton
      to='/~settings'
      text="Back to System Preferences"
      display={['flex', 'none']}
      p={4}
      pb={0}
    />
  );
}
