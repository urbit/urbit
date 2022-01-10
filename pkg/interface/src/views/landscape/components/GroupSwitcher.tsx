import {
    Box,
    Col,

    Icon, Row,
    Text
} from '@tlon/indigo-react';
import React from 'react';
import { Link } from 'react-router-dom';
import { useLocalStorageState } from '~/logic/lib/useLocalStorageState';
import { uxToHex } from '~/logic/lib/util';
import { getGroupFromWorkspace, getTitleFromWorkspace } from '~/logic/lib/workspace';
import useMetadataState from '~/logic/state/metadata';
import { Workspace } from '~/types/workspace';
import { Dropdown } from '~/views/components/Dropdown';
import { MetadataIcon } from './MetadataIcon';
import { TitleActions } from './Sidebar/TitleActions';
import { SidebarListConfig } from './Sidebar/types';

const GroupSwitcherItem = ({ to, children, bottom = false, ...rest }) => (
  <Link to={to}>
    <Box
      py={1}
      {...rest}
      borderBottom={bottom ? 0 : 1}
      borderBottomColor="lightGray"
    >
      <Row p={2} alignItems="center">
        {children}
      </Row>
    </Box>
  </Link>
);

function RecentGroups(props: { recent: string[] }) {
  const { recent } = props;
  if (recent.length < 2) {
    return null;
  }
  const associations = useMetadataState(state => state.associations);

  return (
    <Col borderBottom={1} borderBottomColor="lightGray" p={1}>
      <Box fontSize={0} px={1} py={2} color="gray">
        Recent Groups
      </Box>
      {props.recent.filter((e) => {
        return (e in associations?.groups);
      }).slice(0, 4).map((g) => {
        const assoc = associations.groups[g];
        const color = uxToHex(assoc?.metadata?.color || '0x0');
        return (
          <Link key={g} style={{ minWidth: 0 }} to={`/~landscape${g}`}>
          <Row px={1} pb={2} alignItems="center">
            <Box
              borderRadius={1}
              border={1}
              borderColor="lightGray"
              height="16px"
              width="16px"
              bg={`#${color}`}
              mr={2}
              display="block"
              flexShrink={0}
            />
              <Text verticalAlign='top' maxWidth='100%' overflow='hidden' display='inline-block' style={{ textOverflow: 'ellipsis', whiteSpace: 'pre' }}>{assoc?.metadata?.title}</Text>
            </Row>
          </Link>
        );
      })}
    </Col>
  );
}

export function GroupSwitcher(props: {
  workspace: Workspace;
  baseUrl: string;
  recentGroups: string[];
  isAdmin: any;
}) {
  const { workspace, isAdmin } = props;
  const associations = useMetadataState(state => state.associations);
  const title = getTitleFromWorkspace(associations, workspace);
  const groupPath = getGroupFromWorkspace(workspace);
  const metadata = (workspace.type === 'home' || workspace.type  === 'uqbar-home' || workspace.type  === 'messages')
    ? undefined
    : associations.groups[workspace.group].metadata;
  const navTo = (to: string) => `${props.baseUrl}${to}`;
  const [config, setConfig] = useLocalStorageState<SidebarListConfig>(
    `group-config:${groupPath || 'home'}`,
    {
      sortBy: 'lastUpdated',
      hideUnjoined: false
    }
  );
  return (
    <Row
      width="100%"
      alignItems="center"
      flexShrink={0}
      height='48px'
      backgroundColor="white"
      position="sticky"
      top="0px"
      pl={3}
      borderBottom='1px solid'
      borderColor='lightGray'
    >
      <Col
        bg="white"
        width="100%"
        height="100%"
      >
        <Row flexGrow={1} alignItems="center" justifyContent="space-between">
          <Dropdown
            width="auto"
            dropWidth="231px"
            alignY="top"
            options={
              <Col
                borderRadius={1}
                border={1}
                borderColor="lightGray"
                bg="white"
                width="100%"
                alignItems="stretch"
              >
                  <GroupSwitcherItem to="">
                  <Icon
                    mr={2}
                    color="gray"
                    display="block"
                    icon="Groups"
                  />
                  <Text>All Groups</Text>
                </GroupSwitcherItem>
                <RecentGroups
                  recent={props.recentGroups}
                />
                <GroupSwitcherItem to="/~landscape/new">
                  <Icon mr={2} color="gray" icon="CreateGroup" />
                  <Text> New Group</Text>
                </GroupSwitcherItem>
                <GroupSwitcherItem to="/~landscape/join">
                  <Icon mr={2} color="gray" icon="Plus" />
                  <Text> Join Group</Text>
                </GroupSwitcherItem>
                {workspace.type === 'group' && (
                  <>
                    <GroupSwitcherItem to={navTo('/popover/participants')}>
                      <Icon
                        mr={2}
                        color="gray"
                        icon="Node"
                      />
                      <Text> Participants</Text>
                    </GroupSwitcherItem>
                    <GroupSwitcherItem to={navTo('/popover/settings')}>
                      <Icon
                        mr={2}
                        color="gray"
                        icon="Gear"
                      />
                      <Text> Group Settings</Text>
                    </GroupSwitcherItem>
                    {isAdmin && (<GroupSwitcherItem bottom to={navTo('/invites')}>
                      <Icon
                        mr={2}
                        color="blue"
                        icon="Users"
                      />
                      <Text color="blue">Invite to group</Text>
                    </GroupSwitcherItem>)}
                  </>
                )}
              </Col>
            }
          >
            <Row flexGrow={1} alignItems="center" justifyContent="space-between" width='100%' minWidth={0} flexShrink={0}>
              <Row flexGrow={1} alignItems="center" width='100%' minWidth={0} flexShrink={0}>
                { metadata && <MetadataIcon flexShrink={0} mr={2} metadata={metadata} height="24px" width="24px" /> }
                <Text flexShrink={1} lineHeight="1.1" fontSize={2} fontWeight="600" overflow='hidden' display='inline-block' style={{ textOverflow: 'ellipsis', whiteSpace: 'pre' }}>{title}</Text>
              </Row>
              <TitleActions
                baseUrl={props.baseUrl}
                initialValues={config}
                handleSubmit={setConfig}
                workspace={workspace}
              />
            </Row>
          </Dropdown>
          <Row pr={3} verticalAlign="middle">
            {(workspace.type === 'group') && (
              <>
                {isAdmin && (<Link to={navTo('/invites')}>
                  <Icon
                    display="inline-block"
                    color='blue'
                    icon="Users"
                    ml='12px'
                  />
                </Link>)}
                <Link to={navTo('/popover/settings')}>
                  <Icon color='gray' display="inline-block" ml={'12px'} icon="Gear" />
                </Link>
              </>
            )}
          </Row>
        </Row>
      </Col>
    </Row>
  );
}
