import React, { ReactElement, useEffect, useRef, useState } from 'react';
import { useHistory } from 'react-router-dom';

import {
  Center,
  Box,
  Row,
  BaseImage,
  Text
} from "@tlon/indigo-react";

import RichText from '~/views/components/RichText'
import useSettingsState, {selectCalmState} from "~/logic/state/settings";
import { Sigil } from '~/logic/lib/sigil';
import { ViewProfile } from './ViewProfile';
import { EditProfile } from './EditProfile';
import { SetStatusBarModal } from '~/views/components/SetStatusBarModal';
import { uxToHex } from '~/logic/lib/util';
import { useTutorialModal } from '~/views/components/useTutorialModal';
import useContactState from '~/logic/state/contacts';



export function Profile(props: any): ReactElement {
  const { hideAvatars } = useSettingsState(selectCalmState);
  const history = useHistory();
  const nackedContacts = useContactState(state => state.nackedContacts);

  if (!props.ship) {
    return null;
  }
  const { contact, hasLoaded, isPublic, isEdit, ship } = props;
  const nacked = nackedContacts.has(ship);

  useEffect(() => {
    if(hasLoaded && !contact && !nacked) {
      props.api.contacts.retrieve(ship);
    }
  }, [hasLoaded, contact]);

  const hexColor = contact?.color ? `#${uxToHex(contact.color)}` : '#000000';
  const cover = (contact?.cover)
    ? <BaseImage src={contact.cover} width='100%' height='100%' style={{ objectFit: 'cover' }} />
    : <Box display="block" width='100%' height='100%' backgroundColor='washedGray' />;

  const image = (!hideAvatars && contact?.avatar)
    ? <BaseImage src={contact.avatar} width='100%' height='100%' style={{ objectFit: 'cover' }} />
    : <Sigil padding={24} ship={ship} size={128} color={hexColor} />;

  const anchorRef = useRef<HTMLElement | null>(null);

  useTutorialModal('profile', ship === `~${window.ship}`, anchorRef.current);

  return (
    <Center
      p={[0,4]}
      height="100%"
      width="100%"
    >

      <Box
        ref={anchorRef}
        maxWidth="600px"
        width="100%"
      >
        <Row alignItems="center" justifyContent="space-between">
          <Row>
          {ship === `~${window.ship}` ? (
            <>
            <Text
              py='2'
              cursor='pointer'
              onClick={() => {
 history.push(`/~profile/${ship}/edit`);
}}
            >
              Edit Profile
            </Text>
              <SetStatusBarModal
                py='2'
                ml='3'
                api={props.api}
                ship={`~${window.ship}`}
                contact={contact}
              />
              </>
          ) : null}
          </Row>
          <RichText mb='0' py='2' disableRemoteContent maxWidth='18rem' overflowX='hidden' textOverflow="ellipsis"
            whiteSpace="nowrap"
            overflow="hidden" display="inline-block" verticalAlign="middle">{contact?.status ?? ""}</RichText>
        </Row>
        <Row  width="100%" height="300px">
          {cover}
        </Row>
        <Row
          pb={2}
          alignItems="center"
          width="100%"
        >
          <Center width="100%" marginTop="-48px">
            <Box height='128px' width='128px' borderRadius="2" overflow="hidden">
              {image}
            </Box>
          </Center>
        </Row>
        { isEdit ? (
          <EditProfile
            ship={ship}
            contact={contact}
            api={props.api}
          />
        ) : (
          <ViewProfile
            api={props.api}
            nacked={nacked}
            ship={ship}
            contact={contact}
          />
        ) }
      </Box>
    </Center>
  );
}
