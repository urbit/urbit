import React from "react";
import { Route, Link } from "react-router-dom";
import Helmet from 'react-helmet';

import { Box, Text, Row, Col, Icon, BaseImage } from "@tlon/indigo-react";

import { Sigil } from "~/logic/lib/sigil";
import { uxToHex } from "~/logic/lib/util";

import { ContactCard } from "./components/ContactCard";
import useLocalState from "~/logic/state/local";

export default function ProfileScreen(props: any) {
  const { ship, dark } = props;
  const hideAvatars = useLocalState(state => state.hideAvatars);
  return (
    <>
    <Helmet defer={false}>
      <title>OS1 - Profile</title>
    </Helmet>
    <Route
      path={"/~profile"}
      render={({ match, history }) => {
        const contact = props.contacts?.[window.ship];
        const sigilColor = contact?.color
          ? `#${uxToHex(contact.color)}`
          : dark
          ? "#FFFFFF"
          : "#000000";

        if(!contact) {
          return null;
        }
        return (
          <Box height="100%" px={[0, 3]} pb={[0, 3]} borderRadius={1}>
            <Box
              height="100%"
              width="100%"
              display="grid"
              gridTemplateColumns={["100%", "400px 1fr"]}
              gridTemplateRows={["48px 1fr", "1fr"]}
              borderRadius={1}
              bg="white"
              border={1}
              borderColor="washedGray"
            >
              <Box overflowY="auto" flexGrow={1}>
                <ContactCard
                  contact={contact}
                  path="/~/default"
                  api={props.api}
                  s3={props.s3}
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
