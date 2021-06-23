import { BaseImage, Box, Center, Row, Text } from '@tlon/indigo-react';
import { retrieve } from '@urbit/api';
import React, { ReactElement, useEffect, useRef } from 'react';
import useContactState from '~/logic/state/contact';
import RichText from '~/views/components/RichText';
import { SetStatusBarModal } from '~/views/components/SetStatusBarModal';
import { useTutorialModal } from '~/views/components/useTutorialModal';
import { EditProfile } from './EditProfile';
import { ViewProfile } from './ViewProfile';
import airlock from '~/logic/api';
import { TextLink } from '~/views/components/Link';
import { ShipImage } from '~/views/components/ShipImage';

export function ProfileHeader(props: any): ReactElement {
  return (
    <Box
      border='1px solid'
      borderColor='lightGray'
      borderRadius={3}
      overflow='hidden'
      marginBottom='calc(64px + 2rem)'
    >
      {props.children}
    </Box>
  );
}

export function ProfileImages(props: any): ReactElement {
  const { contact, hideCover, ship } = props;

  const anchorRef = useRef<HTMLDivElement>(null);

  useTutorialModal('profile', ship === `~${window.ship}`, anchorRef);

  const cover =
    contact?.cover && !hideCover ? (
      <BaseImage
        src={contact.cover}
        width='100%'
        height='100%'
        referrerPolicy="no-referrer"
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
        borderRadius={3}
        overflow='hidden'
        position='absolute'
        left='50%'
        marginTop='-64px'
        marginLeft='-64px'
      >
        <ShipImage size={128} sigilSize={80} ship={ship} />
      </Box>
    </>
  );
}

export function ProfileControls(props: any): ReactElement {
  return (
    <Row alignItems='center' justifyContent='space-between' px={3}>
      {props.children}
    </Row>
  );
}

export function ProfileStatus(props: any): ReactElement {
  const { contact } = props;
  return (
    <RichText
      mb={0}
      py={2}
      disableRemoteContent
      maxWidth='18rem'
      overflowX='hidden'
      textOverflow='ellipsis'
      whiteSpace='nowrap'
      overflow='hidden'
      display='inline-block'
      verticalAlign='middle'
      color='gray'
      title={contact?.status ?? ''}
    >
      {contact?.status ?? ''}
    </RichText>
  );
}

export function ProfileActions(props: any): ReactElement {
  const { ship, isPublic, contact } = props;
  return (
    <Row>
      {ship === `~${window.ship}` ? (
        <>
          <TextLink
            py={2}
            fontWeight='500'
            fontSize={1}
            to={`/~profile/${ship}/edit`}
          >
            Edit
            <Text
              fontWeight='500'
              display={['none','inline']}
            >
                {isPublic ? ' Public' : ' Private'} Profile
            </Text>
          </TextLink>
          <SetStatusBarModal
            isControl
            py={2}
            ml={3}
            ship={`~${window.ship}`}
            contact={contact}
          />
        </>
      ) : (
        <>
          <TextLink
            py={2}
            fontWeight='500'
            to={`/~landscape/messages/dm/${ship}`}
          >
            Message
          </TextLink>
        </>
      )}
    </Row>
  );
}

export function Profile(props: any): ReactElement | null {
  const nackedContacts = useContactState(state => state.nackedContacts);

  const { contact, hasLoaded, isEdit, ship } = props;
  const nacked = nackedContacts.has(ship);

  useEffect(() => {
    if (hasLoaded && !contact && !nacked) {
      airlock.poke(retrieve(ship));
    }
  }, [hasLoaded, contact]);

  if (!props.ship) {
    return null;
  }

  return (
    <Center p={[3, 4]} height='100%' width='100%'>
      <Box maxWidth='600px' width='100%' position='relative'>
        { isEdit ? (
          <EditProfile
            ship={ship}
            contact={contact}
          />
        ) : (
          <ViewProfile
            nacked={nacked}
            ship={ship}
            contact={contact}
          />
        )}
      </Box>
    </Center>
  );
}
