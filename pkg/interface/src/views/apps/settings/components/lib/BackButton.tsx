import React from 'react';
import { Link } from 'react-router-dom';
import { Text } from '@tlon/indigo-react';

export function BackButton(props: {}) {
  return (
    <Link to='/~settings'>
      <Text
        display={['block', 'none']}
        fontSize='2'
        fontWeight='medium'
        p={4}
        pb={0}
      >
        {'<- Back to System Preferences'}
      </Text>
    </Link>
  );
}
