import React from "react";
import { Route, Link } from "react-router-dom";
import Helmet from 'react-helmet';

import { Box, Text, Row, Col, Icon, BaseImage } from "@tlon/indigo-react";

import { uxToHex } from "~/logic/lib/util";

import { Profile } from "./components/Profile";
import useLocalState from "~/logic/state/local";

export default function ProfileScreen(props: any) {
  const { dark } = props;
  const hideAvatars = useLocalState(state => state.hideAvatars);
  return (
    <>
    <Helmet defer={false}>
      <title>{ props.notificationsCount ? `(${String(props.notificationsCount) }) `: '' }Landscape - Profile</title>
    </Helmet>
    <Route
      path={"/~profile/:ship/:edit?"}
      render={({ match, history }) => {
        const ship = match.params.ship;
        const isEdit = match.url.includes('edit');
        const contact = props.contacts?.[`~${ship}`];
        console.log(props.contacts);
        const sigilColor = contact?.color
          ? `#${uxToHex(contact.color)}`
          : dark
          ? "#FFFFFF"
          : "#000000";

        return (
          <Box height="100%" px={[0, 3]} pb={[0, 3]} borderRadius={1}>
            <Box
              height="100%"
              width="100%"
              borderRadius={1}
              bg="white"
              border={1}
              borderColor="washedGray"
            >
              <Box overflowY="auto" flexGrow={1}>
                <Profile
                  ship={ship}
                  contact={contact}
                  api={props.api}
                  s3={props.s3}
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
