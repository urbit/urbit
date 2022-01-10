import { Box, Row, Text } from '@tlon/indigo-react';
import React, { ReactElement } from 'react';
import { useHistory } from 'react-router-dom';
import { getGroupFromWorkspace } from '~/logic/lib/workspace';
import useHarkState from '~/logic/state/hark';
import useMetadataState from '~/logic/state/metadata';
import { Workspace } from '~/types/workspace';
import { SidebarListConfig } from './types';
import { getFeedPath } from '~/logic/lib/util';

export function SidebarListHeader(props: {
  initialValues: SidebarListConfig;
  baseUrl: string;
  selected: string;
  workspace: Workspace;
  handleSubmit: (s: any) => void;
}): ReactElement {
  const history = useHistory();

  const groupPath = getGroupFromWorkspace(props.workspace);
  const associations = useMetadataState(state => state.associations);

  const feedPath = groupPath ? getFeedPath(associations.groups[groupPath]) : undefined;

  const unreadCount = useHarkState(
    s => s.unreads?.graph?.[feedPath ?? '']?.['/']?.unreads as number ?? 0
  );

  return (
    <Box>
    {( feedPath) ? (
       <Row
         flexShrink={0}
         alignItems="center"
         justifyContent="space-between"
         py={2}
         px={3}
         height='48px'
         borderBottom={1}
         borderColor="lightGray"
         backgroundColor={['transparent',
           history.location.pathname.includes(`/~landscape${groupPath}/feed`)
           ? (
            'washedGray'
           ) : (
            'transparent'
           )]}
           cursor={(
             history.location.pathname === `/~landscape${groupPath}/feed`
             ? 'default' : 'pointer'
           )}
         onClick={() => {
           history.push(`/~landscape${groupPath}/feed`);
         }}
       >
         <Text>
           Group Feed
         </Text>
         <Text mr={1} color="blue">
           { unreadCount > 0 && unreadCount}
         </Text>
       </Row>
     ) : <Box mt={2} />
    }
    </Box>
  );
}
