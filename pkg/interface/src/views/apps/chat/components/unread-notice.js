import React from 'react';
import moment from 'moment';
import { Box, Text } from '@tlon/indigo-react';

export const UnreadNotice = (props) => {
  const { unreadCount, unreadMsg, dismissUnread, onClick } = props;

  if (!unreadMsg || (unreadCount === 0)) {
    return null;
  }

  let datestamp = moment.unix(unreadMsg.when / 1000).format('YYYY.M.D');
  const timestamp = moment.unix(unreadMsg.when / 1000).format('HH:mm');

  if (datestamp === moment().format('YYYY.M.D')) {
    datestamp = null;
  }

  return (
    <Box style={{ left: '0px' }}
      p='4'
      width='100%'
      position='absolute'
      zIndex='1'
      className='unread-notice'
    >
      <Box
        backgroundColor='white'
        display='flex'
        alignItems='center'
        p='2'
        fontSize='0'
        justifyContent='space-between'
        borderRadius='1'
        border='1'
        borderColor='blue'>
        <Text flexShrink='0' display='block' cursor='pointer' onClick={onClick}>
          {unreadCount} new messages since{' '}
          {datestamp && (
            <>
              <Text color='blue'>~{datestamp}</Text> at{' '}
            </>
          )}
          <Text color='blue'>{timestamp}</Text>
        </Text>
        <Text
          ml='4'
          color='blue'
          cursor='pointer'
          textAlign='right'
          onClick={dismissUnread}>
          Mark as Read
        </Text>
      </Box>
    </Box>
  );
}