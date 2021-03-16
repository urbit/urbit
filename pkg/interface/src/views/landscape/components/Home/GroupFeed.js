import React from 'react';
import { Box, Col, Row, Text } from '@tlon/indigo-react';
import { Link } from 'react-router-dom';


export function GroupFeedHeader(props) {
  const { baseUrl } = props;

  return (
    <Row 
      width="100%"
      height="48px"
      pl="2"
      pr="2"
      alignItems="center"
      borderBottom={1}
      borderColor="washedGray">
      <Link to={baseUrl}><Text>{'<- Back'}</Text></Link>
    </Row>
  );
}

export function GroupFeed(props) {
  const { baseUrl } = props;

  return (
    <Box width="100%" height="100%">
      <GroupFeedHeader baseUrl={baseUrl} />
      <Row width="100%">
      </Row> 
    </Box>
  );
}


