import React from 'react';
import { Route } from 'react-router-dom';
import Helmet from 'react-helmet';

import { Box } from '@tlon/indigo-react';

import { Profile } from './components/Profile';
import useHarkState from '~/logic/state/hark';
import useContactState from '~/logic/state/contacts';

export default function ProfileScreen(props: any) {
  const notificationsCount = useHarkState(state => state.notificationsCount);
  const contacts = useContactState(state => state.contacts);
  return (
    <>
    <Helmet defer={false}>
      <title>{ notificationsCount ? `(${String(notificationsCount) }) `: '' }Landscape - Profile</title>
    </Helmet>
    <Route
      path={'/~profile/:ship/:edit?'}
      render={({ match }) => {
        const ship = match.params.ship;
        const isEdit = match.url.includes('edit');
        const isPublic = props.isContactPublic;
        const contact = contacts?.[ship];

        return (
          <Box height="100%" px={[0, 3]} pb={[0, 3]} borderRadius={1}>
            <Box
              height="100%"
              width="100%"
              borderRadius={1}
              bg="white"
              border={1}
              borderColor="washedGray"
              overflowY="auto"
              flexGrow
            >
              <Box>
                <Profile
                  ship={ship}
                  hasLoaded={Object.keys(contacts).length !== 0}
                  contact={contact}
                  s3={props.s3}
                  isEdit={isEdit}
                  isPublic={isPublic}
                  nackedContacts={props.nackedContacts}
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
