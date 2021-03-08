import React, { useCallback } from 'react';

import { Box, Col, Button, Text } from '@tlon/indigo-react';
import { Group } from '@urbit/api/groups';
import { Association, Associations } from '@urbit/api/metadata';
import { GroupNotificationsConfig } from '@urbit/api';

import GlobalApi from '~/logic/api/global';
import { GroupAdminSettings } from './Admin';
import { GroupPersonalSettings } from './Personal';
import { GroupChannelSettings } from './Channels';
import { useHistory } from 'react-router-dom';
import { resourceFromPath, roleForShip } from '~/logic/lib/group';
import { StorageState } from '~/types';

const Section = ({ children }) => (
  <Box boxShadow="inset 0px 1px 0px rgba(0, 0, 0, 0.2)">{children}</Box>
);

interface GroupSettingsProps {
  group: Group;
  association: Association;
  api: GlobalApi;
  baseUrl: string;
}
export function GroupSettings(props: GroupSettingsProps) {
  const history = useHistory();

  const linkRelative = useCallback(
    (url: string) =>
      useCallback(() => history.push(`${props.baseUrl}${url}`), [url]),
    [history, props.baseUrl]
  );

  const isAdmin =
    resourceFromPath(props.association.group).ship.slice(1) === window.ship ||
    roleForShip(props.group, window.ship) === 'admin';

  return (
    <Box height="100%" overflowY="auto">
      <Col>
        <GroupPersonalSettings {...props} />
        <Section>
          <Col p="4" maxWidth="384px">
            <Text fontSize="2" fontWeight="600">
              Participants
            </Text>
            <Text gray>View list of all group participants and statuses</Text>
            <Button primary mt="4" onClick={linkRelative('/participants')}>View List</Button>
          </Col>
        </Section>
        { isAdmin && (
          <>
            <Section>
              <GroupAdminSettings {...props} />
            </Section>
            <Section>
              <GroupChannelSettings {...props} />
            </Section>
          </>
        )}
      </Col>
    </Box>
  );
}
