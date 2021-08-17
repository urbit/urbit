import React, { useState, useEffect } from 'react';

import { Row, Box, Text } from '@tlon/indigo-react';

import { SetStatus } from '~/views/apps/profile/components/SetStatus';

export const SetStatusBarModal = (props) => {
  const { ship, contact, api, isControl, ...rest } = props;
  const [modalShown, setModalShown] = useState(false);

  const handleKeyDown = (event) => {
    if (event.key === 'Escape') {
      setModalShown(false);
    }
  };

  useEffect(() => {
    window.addEventListener('keydown', handleKeyDown);

    return () => {
      window.removeEventListener('keydown', handleKeyDown);
    };
  }, [modalShown]);

  return (
    <>
      {modalShown && (
        <Box
          backgroundColor='scales.black30'
          left='0px'
          top='0px'
          width='100%'
          height='100%'
          zIndex={4}
          position='fixed'
          display='flex'
          justifyContent='center'
          alignItems='center'
          onClick={() => setModalShown(false)}
        >
          <Box
            maxWidth='500px'
            width='100%'
            bg='white'
            borderRadius={2}
            border={[0, 1]}
            borderColor={['washedGray', 'washedGray']}
            onClick={(e) => e.stopPropagation()}
            display='flex'
            alignItems='stretch'
            flexDirection='column'
          >
            <Box m={3}>
              <SetStatus
                ship={ship}
                contact={contact}
                api={api}
                callback={() => {
                  setModalShown(false);
                }}
              />
            </Box>
          </Box>
        </Box>
      )}
      <Row {...rest} flexShrink={0} onClick={() => setModalShown(true)}>
        <Text
          color='black'
          cursor='pointer'
          fontWeight={isControl ? '500' : '400'}
          flexShrink={0}
          fontSize={1}
        >
          Set Status
        </Text>
      </Row>
    </>
  );
};
