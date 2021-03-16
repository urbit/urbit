import React from 'react';
import { Row, Text } from '@tlon/indigo-react';
import { Link } from 'react-router-dom';


export function GroupFeedHeader(props) {
  const { baseUrl, history } = props;

  return (
    <Row 
      flexShrink="1"
      width="100%"
      height="48px"
      pl="2"
      pr="2"
      alignItems="center"
      borderBottom={1}
      borderColor="washedGray">
      { baseUrl !== history.location.pathname ? (
          <Link to={baseUrl}><Text>{'<- Back'}</Text></Link>
        ) : null
      }
    </Row>
  );
}


