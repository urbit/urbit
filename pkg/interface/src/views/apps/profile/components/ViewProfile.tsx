import React, { ReactElement } from 'react';
import _ from 'lodash';
import { useHistory } from 'react-router-dom';
import { Center, Box, Text, Row, Col } from '@tlon/indigo-react';
import RichText from '~/views/components/RichText';
import useSettingsState, { selectCalmState } from '~/logic/state/settings';
import { Sigil } from '~/logic/lib/sigil';
import { GroupLink } from '~/views/components/GroupLink';
import { lengthOrder } from '~/logic/lib/util';
import useLocalState from '~/logic/state/local';
import {
  ProfileHeader,
  ProfileControls,
  ProfileActions,
  ProfileStatus,
  ProfileImages
} from './Profile';
import useContactState from '~/logic/state/contact';

export function ViewProfile(props: any): ReactElement {
  const { hideNicknames } = useSettingsState(selectCalmState);
  const { api, contact, nacked, ship } = props;

  const isPublic = useContactState(state => state.isContactPublic);

  return (
    <>
      <ProfileHeader>
        <ProfileControls>
          <ProfileActions
            ship={ship}
            isPublic={isPublic}
            contact={contact}
            api={props.api}
          />
          <ProfileStatus contact={contact} />
        </ProfileControls>
        <ProfileImages key={ship} contact={contact} ship={ship} />
      </ProfileHeader>
      <Row pb={2} alignItems='center' width='100%'>
        <Center width='100%'>
          <Text fontWeight='500'>
            {!hideNicknames && contact?.nickname ? contact.nickname : ''}
          </Text>
        </Center>
      </Row>
      <Row pb={2} alignItems='center' width='100%'>
        <Center width='100%'>
          <Text mono color='darkGray'>
            {ship}
          </Text>
        </Center>
      </Row>
      <Col pb={2} mt='3' alignItems='center' justifyContent='center' width='100%'>
        <Center flexDirection='column' maxWidth='32rem'>
          <RichText width='100%' disableRemoteContent>
            {contact?.bio ? contact.bio : ''}
          </RichText>
        </Center>
      </Col>
      {(contact?.groups || []).length > 0 && (
        <Col gapY='3' mb='3' mt='6' alignItems='flex-start'>
          <Text gray>Pinned Groups</Text>
          <Col>
            {contact?.groups.sort(lengthOrder).map((g) => (
              <GroupLink
                api={api}
                resource={g}
                measure={() => {}}
              />
            ))}
          </Col>
        </Col>
      )}

      {nacked || (!isPublic && ship === `~${window.ship}`) ? (
        <Box
          height='200px'
          borderRadius={1}
          bg='white'
          border={1}
          borderColor='washedGray'
        >
          <Center height='100%'>
            <Text mono pr={1} color='gray'>
              {ship}
            </Text>
            <Text color='gray'>remains private</Text>
          </Center>
        </Box>
      ) : null}
    </>
  );
}
