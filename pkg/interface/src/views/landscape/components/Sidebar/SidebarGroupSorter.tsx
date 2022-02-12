import { Droppable, Draggable } from 'react-beautiful-dnd';

import { Box, Text } from '@tlon/indigo-react';
import React, { ReactElement, useCallback } from 'react';
import useMetadataState from '~/logic/state/metadata';

export type GroupFolder = { folder: string, groups: string[] };
export type GroupOrder = (string | GroupFolder)[];

function GroupCard({ title, group, index }: { title: string; group: string; index: number }) {
  return (
    <Draggable key={group} draggableId={group} index={index}>
      {provided => (
        <Box py={2} px={3}
          m={2}
          backgroundColor="white"
          borderRadius={2}
          ref={provided.innerRef}
          {...provided.draggableProps}
          {...provided.dragHandleProps}
          overflow="hidden"
          whiteSpace="nowrap"
          textOverflow="ellipsis"
        >
          <Text>{title}</Text>
        </Box>
      )}
    </Draggable>
  );
}

export function SidebarGroupSorter({ groupOrder = [] }: { groupOrder: GroupOrder }): ReactElement {
  const { associations } = useMetadataState();

  const getTitle = useCallback((g: string | GroupFolder) => typeof g === 'string' ? associations.groups[g]?.metadata?.title : g.folder, [associations]);

  return (
    <Droppable droppableId="groups" style={{ width: '100%' }}>
      {provided => (
        <Box {...provided.droppableProps} ref={provided.innerRef} backgroundColor="washedGray" mt="-8px" mb="-4px">
          {groupOrder.map((entry, index) => {
            const title = getTitle(entry);
            if (typeof entry === 'string' && title) {
              return <GroupCard key={entry} title={title} group={entry} index={index} />;
            }
            // TODO: handle folders in groupOrder
            return null;
            // return <Box key={entry.folder}>
            //   <Text>{title}</Text>
            //   {entry.groups.map(g => <GroupCard key={g} title={getTitle(g)} group={g} />)}
            // </Box>;
          })}
          {provided.placeholder}
        </Box>
      )}
    </Droppable>
  );
}
