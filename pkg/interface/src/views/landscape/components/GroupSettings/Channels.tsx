import { Col, Icon, Row, Text } from '@tlon/indigo-react';
import { Association, Group, metadataRemove, metadataUpdate } from '@urbit/api';
import React, { useCallback } from 'react';
import GlobalApi from '~/logic/api/global';
import { resourceFromPath, roleForShip } from '~/logic/lib/group';
import { getModuleIcon, GraphModule } from '~/logic/lib/util';
import useMetadataState from '~/logic/state/metadata';
import { Dropdown } from '~/views/components/Dropdown';
import { StatelessAsyncAction } from '~/views/components/StatelessAsyncAction';
import airlock from '~/logic/api';

interface GroupChannelSettingsProps {
  group: Group;
  association: Association;
  api: GlobalApi;
}

export function GroupChannelSettings(props: GroupChannelSettingsProps) {
  const { association, group } = props;
  const associations = useMetadataState(state => state.associations);
  const channels = Object.values(associations.graph).filter(
    ({ group }) => association.group === group
  );

  const onChange = useCallback(
    async (resource: string, preview: boolean) => {
      return airlock.poke(metadataUpdate(associations.graph[resource], { preview }));
    },
    [associations.graph]
  );

  const onRemove = useCallback(
    async (resource: string) => {
      return airlock.poke(metadataRemove('graph', resource, association.group));
    },
    [association]
  );

  const disabled =
    resourceFromPath(association.group).ship.slice(1) !== window.ship &&
    roleForShip(group, window.ship) !== 'admin';
  return (
    <Col maxWidth="384px" width="100%">
      <Text p={4} id="channels" fontWeight="600" fontSize={2}>
        Channels
      </Text>
      <Text pl={4} gray>Pinning a channel marks it as featured when joining or previewing a group.</Text>
      <Col p={4} width="100%" gapY={3}>
        {channels.filter(({ metadata }) => !metadata.hidden).map(({ resource, metadata }) => (
          <Row justifyContent="space-between" width="100%" key={resource}>
            <Row gapX={2}>
              <Icon
                icon={getModuleIcon(
                  'graph' in metadata?.config
                    ? metadata?.config?.graph as GraphModule
                    : 'post')}
              />
              <Text>{metadata.title}</Text>
              {metadata.preview && <Text gray>Pinned</Text>}
            </Row>
            {!disabled && (
              <Dropdown
                options={
                  <Col
                    bg="white"
                    border={1}
                    borderRadius={1}
                    borderColor="lightGray"
                    p={1}
                    gapY={1}
                  >
                    <StatelessAsyncAction
                      bg="transparent"
                      name={`pin-${resource}`}
                      onClick={() => onChange(resource, !metadata.preview)}
                    >
                      {metadata.preview ? 'Unpin' : 'Pin'}
                    </StatelessAsyncAction>
                    <StatelessAsyncAction
                      bg="transparent"
                      name={`remove-${resource}`}
                      onClick={() => onRemove(resource)}
                    >
                      <Text color="red">Remove from group</Text>
                    </StatelessAsyncAction>
                  </Col>
                }
              >
                <Icon icon="Ellipsis" />
              </Dropdown>
            )}
          </Row>
        ))}
      </Col>
    </Col>
  );
}
