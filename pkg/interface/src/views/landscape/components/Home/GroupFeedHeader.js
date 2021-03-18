import React from 'react';
import { Box, Row, Text } from '@tlon/indigo-react';
import { Link } from 'react-router-dom';


export function GroupFeedHeader(props) {
  const { baseUrl, history } = props;

  return (
    <Row 
      flexShrink="0"
      width="100%"
      height="48px"
      pl="2"
      pr="2"
      alignItems="center"
      borderBottom={1}
      borderColor="washedGray">
      <Box display={['block', 'none']}>
        { baseUrl !== history.location.pathname ? (
          <Link to={baseUrl}>
            <Text pr="2">{'<- Back'}</Text>
          </Link>
          ) : null
        }
      </Box>
      <Text bold fontSize="2" pr="2">Group Feed</Text>
      <Text fontSize="0" p="1" backgroundColor="washedGray">Everyone can post</Text>
    </Row>
  );
}


