import React from 'react';
import { BrowserRouter } from 'react-router-dom';
import { ThemeProvider } from 'styled-components';
import light from './themes/light';
import { Box, Reset } from '@tlon/indigo-react';
import StartupModal from './lib/startupModal.js';
import Body from './lib/body.js';
import { useSettings } from '../hooks/useSettings.js';

const Root = () => {
  const { loaded, wallet, provider } = useSettings();
  const blur = !loaded ? false : !(wallet && provider);

  return (
    <BrowserRouter>
      <ThemeProvider theme={light}>
        <Reset />
        {loaded ? <StartupModal /> : null}
        <Box
          display="flex"
          flexDirection="column"
          position="absolute"
          alignItems="center"
          backgroundColor="lightOrange"
          width="100%"
          minHeight={loaded ? '100%' : 'none'}
          height={loaded ? 'none' : '100%'}
          style={{ filter: blur ? 'blur(8px)' : 'none' }}
          px={[0, 4]}
          pb={[0, 4]}
        >
          <Body />
        </Box>
      </ThemeProvider>
    </BrowserRouter>
  );
};

export default Root;
