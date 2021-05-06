import { Box, Button, Col, Text } from '@tlon/indigo-react';
import { Group } from '@urbit/api/groups';
import { Association } from '@urbit/api/metadata';
import React, { useCallback } from 'react';
import { useHistory } from 'react-router-dom';
import GlobalApi from '~/logic/api/global';
import { resourceFromPath, roleForShip } from '~/logic/lib/group';
import { GroupAdminSettings } from './Admin';
import { GroupChannelSettings } from './Channels';
import { GroupFeedSettings } from './GroupFeed';
import { GroupPersonalSettings } from './Personal';

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

  const isOwner =
    resourceFromPath(props.association.group).ship.slice(1) === window.ship;

  const isAdmin =
    isOwner || roleForShip(props.group, window.ship) === 'admin';

  return (
    <Box height="100%" overflowY="auto">
      <Col>
        <GroupPersonalSettings {...props} />
        <Section>
          <Col p={4} maxWidth="384px">
            <Text fontSize={2} fontWeight="600">
              Participants
            </Text>
            <Text gray>View list of all group participants and statuses</Text>
            <Button primary mt={4} onClick={linkRelative('/participants')}>View List</Button>
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
            { isOwner && (
              <Section>
                <GroupFeedSettings {...props} />
              </Section>
            )}
          </>
        )}
      </Col>
    </Box>
  );
}
