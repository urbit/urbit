import React from 'react';
import { Route, Link } from 'react-router-dom';
import Helmet from 'react-helmet';

import { Box } from '@tlon/indigo-react';

import { Profile } from './components/Profile';
import useContactState from '~/logic/state/contact';
import useHarkState from '~/logic/state/hark';

export default function ProfileScreen(props: any) {
  const contacts = useContactState(state => state.contacts);
  const notificationsCount = useHarkState(state => state.notificationsCount);
  return (
    <>
      <Helmet defer={false}>
        <title>
          {notificationsCount
            ? `(${String(notificationsCount)}) `
            : ''}
          Landscape - Profile
        </title>
      </Helmet>
      <Route
        path={'/~profile/:ship/:edit?'}
        render={({ match }) => {
          const ship = match.params.ship;
          const isEdit = match.url.includes('edit');
          const contact = contacts?.[ship];

          return (
            <Box height='100%' px={[0, 3]} pb={[0, 3]} borderRadius={2}>
              <Box
                height='100%'
                width='100%'
                borderRadius={2}
                bg='white'
                border={1}
                borderColor='washedGray'
                overflowY='auto'
                flexGrow
              >
                <Box>
                  <Profile
                    ship={ship}
                    hasLoaded={Object.keys(contacts).length !== 0}
                    contact={contact}
                    api={props.api}
                    isEdit={isEdit}
                  />
                </Box>
              </Box>
            </Box>
          );
        }}
      />
    </>
  );
}
