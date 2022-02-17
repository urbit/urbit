import { Association, deSig, Group } from '@urbit/api';
import { Box, Button, Col, Text,
  ManagedCheckboxField as Checkbox, ManagedRadioButtonField as Radio, Icon, Row
} from '@tlon/indigo-react';
import React, { useCallback } from 'react';
import { Link, useHistory } from 'react-router-dom';
import { resourceFromPath, roleForShip } from '~/logic/lib/group';
import { useLocalStorageState } from '~/logic/lib/useLocalStorageState';
import { FormikOnBlur } from '~/views/components/FormikOnBlur';
import { SidebarListConfig } from '../Sidebar/types';
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
  baseUrl: string;
}
export function GroupSettings(props: GroupSettingsProps) {
  const history = useHistory();

  const linkRelative = useCallback(
    (url: string) =>
      useCallback(() => history.push(`${props.baseUrl}${url}`), [url]),
    [history, props.baseUrl]
  );
  const groupPath = props.association?.group;

  const [config, setConfig] = useLocalStorageState<SidebarListConfig>(
    `group-config:${groupPath || 'home'}`,
    {
      sortBy: 'lastUpdated',
      hideUnjoined: false
    }
  );

  const isOwner =
    deSig(resourceFromPath(props.association.group).ship) === window.ship;

  const isAdmin =
    isOwner || roleForShip(props.group, window.ship) === 'admin';

  return (
    <Box height="100%" overflowY="auto">
      <Col>
        <GroupPersonalSettings {...props} />
        <Section>
          <FormikOnBlur initialValues={config} onSubmit={setConfig}>
            <Col p={4} maxWidth="384px">
              <Text fontSize={2} fontWeight="600">
                Channels
              </Text>
              <Box mt="16px" mb="8px">
                <Text color="gray">Sort Order</Text>
              </Box>
              <Radio mb={1} label="A -> Z" id="asc" name="sortBy" />
              <Radio label="Last Updated" id="lastUpdated" name="sortBy" />
              <Checkbox
                my={3}
                id="hideUnjoined"
                label="Hide Unsubscribed Channels"
              />
            </Col>
          </FormikOnBlur>
        </Section>
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
