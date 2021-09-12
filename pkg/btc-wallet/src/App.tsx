import React from 'react';
import { BrowserRouter } from 'react-router-dom';
import { ThemeProvider } from 'styled-components';
import light from './themes/light';
import { Box, Reset } from '@tlon/indigo-react';
import StartupModal from './components/StartupModal';
import { useSettings } from './hooks/useSettings';
import Body from './components/Body';
import Header from './components/Header';

const App: React.FC = () => {
  const { loaded, wallet, provider, scanProgress } = useSettings();
  const scanning = scanProgress?.main !== null || scanProgress?.change !== null;
  const blur = !loaded || scanning ? false : !(wallet && provider);

  return (
    <BrowserRouter>
      <ThemeProvider theme={light}>
        <Reset />
        {loaded && !scanning ? <StartupModal /> : null}
        <Box
          display="flex"
          flexDirection="column"
          position="absolute"
          alignItems="center"
          backgroundColor="lightOrange"
          width="100%"
          minHeight={loaded && !scanning ? '100%' : 'none'}
          height={loaded && !scanning ? 'none' : '100%'}
          style={{ filter: blur ? 'blur(8px)' : 'none' }}
        >
          <Header />
          <Body />
        </Box>
      </ThemeProvider>
    </BrowserRouter>
  );
};

export default App;
