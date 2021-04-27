import React, { useEffect, useState } from 'react';
import moment from 'moment';
import { Box, Text, Center, Icon } from '@tlon/indigo-react';
import VisibilitySensor from 'react-visibility-sensor';

import Timestamp from '~/views/components/Timestamp';

export const UnreadNotice = (props) => {
  const { unreadCount, unreadMsg, dismissUnread, onClick } = props;

  if (unreadCount === 0) {
    return null;
  }

  const stamp = unreadMsg && moment.unix(unreadMsg.post['time-sent'] / 1000);

  return (
    <Box
      style={{ left: '0px', top: '0px' }}
      p='12px'
      width='100%'
      position='absolute'
      zIndex='1'
      className='unread-notice'
    >
      <Center>
        <Box backgroundColor='white' borderRadius='3' overflow='hidden'>
          <Box
            backgroundColor='washedBlue'
            display='flex'
            alignItems='center'
            p='2'
            fontSize='0'
            justifyContent='space-between'
            borderRadius='3'
            border='1'
            borderColor='lightBlue'
          >
            <Text
              textOverflow='ellipsis'
              whiteSpace='pre'
              overflow='hidden'
              display='flex'
              cursor={unreadMsg ? 'pointer' : null}
              onClick={onClick}
            >
              {unreadCount} new message{unreadCount > 1 ? 's' : ''} 
              {unreadMsg && (
                <>
                since{' '}
                <Timestamp stamp={stamp} color='black' date={true} fontSize={1} />
                </>
              )}
            </Text>
            <Icon
              icon='X'
              ml={unreadMsg ? 4 : 1}
              color='black'
              cursor='pointer'
              textAlign='right'
              onClick={dismissUnread}
            />
          </Box>
        </Box>
      </Center>
    </Box>
  );
};
