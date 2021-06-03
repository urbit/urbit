import {
  Box,

  Center, Col, Icon,

  ToggleSwitch, Text,
  StatelessToggleSwitchField
} from '@tlon/indigo-react';
import { Association, GraphConfig, resourceFromPath } from '@urbit/api';
import { useField } from 'formik';
import _ from 'lodash';
import React, { useEffect, useState } from 'react';
import { isWatching } from '~/logic/lib/hark';
import { getModuleIcon, GraphModule } from '~/logic/lib/util';
import useGraphState from '~/logic/state/graph';
import useHarkState from '~/logic/state/hark';
import useMetadataState, { useGraphsForGroup } from '~/logic/state/metadata';
import { MetadataIcon } from '~/views/landscape/components/MetadataIcon';

export function GroupChannelPicker(props: {}) {
  const associations = useMetadataState(s => s.associations);

  return (
    <Col gapY={3}>
      {_.map(associations.groups, (assoc: Association, group: string) => (
        <GroupWithChannels key={group} association={assoc} />
      ))}
    </Col>
  );
}

function GroupWithChannels(props: { association: Association }) {
  const { association } = props;
  const { metadata } = association;

  const groupWatched = useHarkState(s =>
    s.notificationsGroupConfig.includes(association.group)
  );

  const [{ value }, meta, { setValue }] = useField(
    `groups["${association.group}"]`
  );

  const onChange = () => {
    setValue(!value);
  };

  useEffect(() => {
    setValue(groupWatched);
  }, []);

  const graphs = useGraphsForGroup(association.group);
  const joinedGraphs = useGraphState(s => s.graphKeys);
  const joinedGroupGraphs = _.pickBy(graphs, (_, graph: string) => {
    const { ship, name } = resourceFromPath(graph);
    return joinedGraphs.has(`${ship.slice(1)}/${name}`);
  });

  const [open, setOpen] = useState(false);

  return (
    <Box
      display="grid"
      gridTemplateColumns="24px 24px 1fr 24px 24px"
      gridTemplateRows="auto"
      gridGap={2}
      gridTemplateAreas="'arrow icon title graphToggle groupToggle'"
    >
      {Object.keys(joinedGroupGraphs).length > 0 && (
        <Center
          cursor="pointer"
          onClick={() => setOpen(o => !o)}
          gridArea="arrow"
        >
          <Icon icon={open ? 'ChevronSouth' : 'ChevronEast'} />
        </Center>
      )}
      <MetadataIcon
        size="24px"
        gridArea="icon"
        metadata={association.metadata}
      />
      <Box gridArea="title">
        <Text>{metadata.title}</Text>
      </Box>
      <Box gridArea="groupToggle">
        <StatelessToggleSwitchField selected={value} onChange={onChange} />
      </Box>
      {open &&
        _.map(joinedGroupGraphs, (a: Association, graph: string) => (
          <Channel key={graph} association={a} />
        ))}
    </Box>
  );
}
function Channel(props: { association: Association }) {
  const { association } = props;
  const { metadata } = association;
  const watching = useHarkState((s) => {
    const config = s.notificationsGraphConfig;
    return isWatching(config, association.resource);
  });

  const [{ value }, meta, { setValue, setTouched }] = useField(
    `graph["${association.resource}"]`
  );

  useEffect(() => {
    setValue(watching);
  }, [watching]);

  const onClick = () => {
    setValue(!value);
    setTouched(true);
  }


  const icon = getModuleIcon((metadata.config as GraphConfig)?.graph as GraphModule);

  return (
    <>
      <Center gridColumn={2}>
        <Icon icon={icon} />
      </Center>
      <Box gridColumn={3}>
        <Text> {metadata.title}</Text>
      </Box>
      <Box gridColumn={4}>
        <StatelessToggleSwitchField selected={value} onClick={onClick} />
      </Box>
    </>
  );
}
