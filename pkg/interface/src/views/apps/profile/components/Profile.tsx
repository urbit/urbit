import React, { ReactElement, useEffect, useRef, useState } from 'react';
import { useHistory } from 'react-router-dom';
import { Center, Box, Row, BaseImage, Text } from '@tlon/indigo-react';
import RichText from '~/views/components/RichText';
import useSettingsState, { selectCalmState } from '~/logic/state/settings';
import { Sigil } from '~/logic/lib/sigil';
import { ViewProfile } from './ViewProfile';
import { EditProfile } from './EditProfile';
import { SetStatusBarModal } from '~/views/components/SetStatusBarModal';
import { uxToHex } from '~/logic/lib/util';
import { useTutorialModal } from '~/views/components/useTutorialModal';
import useContactState from '~/logic/state/contact';

export function ProfileHeader(props: any): ReactElement {
  return (
    <Box
      border='1px solid'
      borderColor='washedGray'
      borderRadius='3'
      overflow='hidden'
      marginBottom='calc(64px + 2rem)'
    >
      {props.children}
    </Box>
  );
}

export function ProfileImages(props: any): ReactElement {
  const { hideAvatars } = useSettingsState(selectCalmState);
  const { contact, hideCover, ship } = { ...props };
  const hexColor = contact?.color ? `#${uxToHex(contact.color)}` : '#000000';

  const anchorRef = useRef<HTMLElement | null>(null)

  useTutorialModal('profile', ship === `~${window.ship}`, anchorRef);

  const cover =
    contact?.cover && !hideCover ? (
      <BaseImage
        src={contact.cover}
        width='100%'
        height='100%'
        style={{ objectFit: 'cover' }}
      />
    ) : (
      <Box
        display='block'
        width='100%'
        height='100%'
        backgroundColor='washedGray'
      />
    );

  const image =
    !hideAvatars && contact?.avatar ? (
      <BaseImage
        src={contact.avatar}
        width='100%'
        height='100%'
        style={{ objectFit: 'cover' }}
      />
    ) : (
      <Sigil padding={24} ship={ship} size={128} color={hexColor} />
    );

  return (
    <>
      <Row ref={anchorRef} width='100%' height='400px' position='relative'>
        {cover}
        <Center position='absolute' width='100%' height='100%'>
          {props.children}
        </Center>
      </Row>
      <Box
        height='128px'
        width='128px'
        borderRadius='3'
        overflow='hidden'
        position='absolute'
        left='50%'
        marginTop='-64px'
        marginLeft='-64px'
      >
        {image}
      </Box>
    </>
  );
}

export function ProfileControls(props: any): ReactElement {
  return (
    <Row alignItems='center' justifyContent='space-between' px='3'>
      {props.children}
    </Row>
  );
}

export function ProfileStatus(props: any): ReactElement {
  const { contact } = { ...props };
  return (
    <RichText
      mb='0'
      py='2'
      disableRemoteContent
      maxWidth='18rem'
      overflowX='hidden'
      textOverflow='ellipsis'
      whiteSpace='nowrap'
      overflow='hidden'
      display='inline-block'
      verticalAlign='middle'
      color='gray'
    >
      {contact?.status ?? ''}
    </RichText>
  );
}

export function ProfileActions(props: any): ReactElement {
  const { ship, isPublic, contact, api } = { ...props };
  const history = useHistory();
  return (
    <Row>
      {ship === `~${window.ship}` ? (
        <>
          <Text
            py='2'
            cursor='pointer'
            fontWeight='500'
            onClick={() => {
              history.push(`/~profile/${ship}/edit`);
            }}
          >
            Edit {isPublic ? 'Public' : 'Private'} Profile
          </Text>
          <SetStatusBarModal
            isControl
            py='2'
            ml='3'
            api={api}
            ship={`~${window.ship}`}
            contact={contact}
          />
        </>
      ) : (
        <>
          <Text
            py='2'
            cursor='pointer'
            fontWeight='500'
            onClick={() => history.push(`/~landscape/dm/${ship.substring(1)}`)}
          >
            Message
          </Text>
        </>
      )}
    </Row>
  );
}

export function Profile(props: any): ReactElement | null {
  const { hideAvatars } = useSettingsState(selectCalmState);
  const history = useHistory();
  const nackedContacts = useContactState(state => state.nackedContacts);

  const { contact, hasLoaded, isEdit, ship } = props;
  const nacked = nackedContacts.has(ship);
  const formRef = useRef(null);

  useEffect(() => {
    if (hasLoaded && !contact && !nacked) {
      props.api.contacts.retrieve(ship);
    }
  }, [hasLoaded, contact]);

  const anchorRef = useRef<HTMLElement | null>(null);

  if (!props.ship) {
    return null;
  }

  return (
    <Center p={[0, 4]} height='100%' width='100%'>
      <Box maxWidth='600px' width='100%' position='relative'>
        { isEdit ? (
          <EditProfile
            ship={ship}
            contact={contact}
            api={props.api}
          />
        ) : (
          <ViewProfile
            nacked={nacked}
            ship={ship}
            api={props.api}
            contact={contact}
          />
        )}
      </Box>
    </Center>
  );
}
