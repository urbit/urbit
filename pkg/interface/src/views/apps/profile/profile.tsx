import React from 'react';
import { Route, Link } from 'react-router-dom';
import Helmet from 'react-helmet';

import { Box } from '@tlon/indigo-react';

import { Profile } from "./components/Profile";

export default function ProfileScreen(props: any) {
  const { dark } = props;
  return (
    <>
      <Helmet defer={false}>
        <title>
          {props.notificationsCount
            ? `(${String(props.notificationsCount)}) `
            : ''}
          Landscape - Profile
        </title>
      </Helmet>
      <Route
        path={'/~profile/:ship/:edit?'}
        render={({ match }) => {
          const ship = match.params.ship;
          const isEdit = match.url.includes('edit');
          const isPublic = props.isContactPublic;
          const contact = props.contacts?.[ship];

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
                    hasLoaded={Object.keys(props.contacts).length !== 0}
                    associations={props.associations}
                    groups={props.groups}
                    contact={contact}
                    api={props.api}
                    storage={props.storage}
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
