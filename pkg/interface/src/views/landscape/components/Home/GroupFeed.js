import React from 'react';
import { Text } from '@tlon/indigo-react';
import { Link } from 'react-router-dom';


export function GroupFeed(props) {
  const { baseUrl } = props;
  return (
    <>
      <Link to={baseUrl}><Text>{'<- Back'}</Text></Link>
    </>
  );
}


