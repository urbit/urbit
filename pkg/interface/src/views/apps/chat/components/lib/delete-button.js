import React, { memo } from 'react';
import { Button, Text, Box } from '@tlon/indigo-react';

export const DeleteButton = memo(({ isOwner, station, changeLoading, association, contacts, api, history }) => {
  const deleteChat = () => {
    changeLoading(
      true,
      true,
      isOwner ? 'Deleting chat...' : 'Leaving chat...',
      () => {
        api.chat.delete(station).then(() => {
          history.push('/~chat');
        });
      }
    );
  };

  const groupPath = association['group-path'];
  const unmanagedVillage = !contacts[groupPath];

  return (
    <Box width='100%'>
      <Box width='100%' mt='3' opacity={(isOwner) ? '0.3' : '1'}>
        <Text fontSize='1' mt='3' display='block' mb='1'>Leave Chat</Text>
        <Text fontSize='0' gray display='block' mb='4'>
          Remove this chat from your chat list.{' '}
          {unmanagedVillage
            ? 'You will need to request for access again'
            : 'You will need to join again from the group page'
          }
        </Text>
        <Button onClick={(!isOwner) ? deleteChat : null}>
          Leave this chat
        </Button>
      </Box>
      <Box width='100%' mt='3' opacity={(isOwner) ? '0.3' : '1'}>
        <Text display='block' fontSize='1' mt='3' mb='1'>Delete Chat</Text>
          <Text display='block' gray fontSize='0' mb='4'>
            Permanently delete this chat.{' '}
            All current members will no longer see this chat.
          </Text>
          <Button destructive onClick={(isOwner) ? deleteChat : null}>Delete this chat</Button>
      </Box>
    </Box>
  );
});

DeleteButton.displayName = 'DeleteButton';
