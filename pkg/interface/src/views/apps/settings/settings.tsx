import React, { ReactElement } from 'react';
import { Route } from 'react-router-dom';
import Helmet from 'react-helmet';

import { Box } from '@tlon/indigo-react';

import Settings from './components/settings';

export default function SettingsScreen(props: any): ReactElement {
  return (
    <>
      <Helmet defer={false}>
        <title>Landscape - Settings</title>
      </Helmet>
      <Route
        path={['/~settings']}
        render={() => {
          return (
            <Box height="100%"
                width="100%"
                px={[0, 3]}
                pb={[0, 3]}
                borderRadius={1}
            >
              <Box
                height="100%"
                width="100%"
                display="grid"
                gridTemplateColumns={['100%', '400px 1fr']}
                gridTemplateRows={['48px 1fr', '1fr']}
                borderRadius={1}
                bg="white"
                border={1}
                borderColor="washedGray"
                overflowY="auto"
                flexGrow
              >
                <Settings {...props} />
              </Box>
            </Box>
          );
        }}
      />
    </>
  );
}
