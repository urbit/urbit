import React from 'react';
import { Box, Row, Text } from '@tlon/indigo-react';
import { Link } from 'react-router-dom';


export function GroupFeedHeader(props) {
  const { baseUrl, history } = props;
  const historyLocation = history.location.pathname;

  const isHome =
    historyLocation === baseUrl ||
    historyLocation === `${baseUrl}/feed`;

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
      <Box display='block'>
        { ( baseUrl !== historyLocation &&
            `${baseUrl}/feed` !== historyLocation
          ) ? (
            <Text pr="2" cursor="pointer" onClick={() => {
              history.goBack();
            }}>{'<- Back'}</Text>
          ) : null
        }
      </Box>
      { isHome ? (
        <>
          <Text bold fontSize="2" pr="2">Group Feed</Text>
          <Text fontSize="0" p="1" backgroundColor="washedGray">Everyone can post</Text>
        </>
      ) : (
        <>
          <Text bold fontSize="2" pr="2">Post by </Text>
          <Text bold fontSize="2" mono>{'TODO'}</Text>
        </>
      )}
    </Row>
  );
}


