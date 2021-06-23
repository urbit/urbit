import React from 'react';
import { TextLink } from '~/views/components/Link';

export function BackButton() {
  return (
      <TextLink
        to="/~settings"
        display={['block', 'none']}
        fontSize={2}
        fontWeight='medium'
        p={4}
        pb={0}
      >
        {'<- Back to System Preferences'}
    </TextLink>
  );
}
