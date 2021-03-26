import React, { useEffect, ReactNode, useCallback } from 'react';
import {
  Switch,
  Route,
  RouteComponentProps
} from 'react-router-dom';
import { Col, Box, Text } from '@tlon/indigo-react';
import _ from 'lodash';
import Helmet from 'react-helmet';

import { AppName } from '@urbit/api';

import { Resource } from './Resource';
import { PopoverRoutes } from './PopoverRoutes';
import { Skeleton } from './Skeleton';
import { InvitePopover } from './InvitePopover';
import { NewChannel } from './NewChannel';

import GlobalApi from '~/logic/api/global';
import { StoreState } from '~/logic/store/type';
import { UnjoinedResource } from '~/views/components/UnjoinedResource';
import { useLocalStorageState } from '~/logic/lib/useLocalStorageState';
import { Loading } from '~/views/components/Loading';

import '~/views/apps/links/css/custom.css';
import '~/views/apps/publish/css/custom.css';
import { getGroupFromWorkspace } from '~/logic/lib/workspace';
import { GroupSummary } from './GroupSummary';
import { Workspace } from '~/types/workspace';
import useContactState from '~/logic/state/contact';
import useGroupState, {useGroupForAssoc} from '~/logic/state/group';
import useHarkState from '~/logic/state/hark';
import useMetadataState, {useAssocForWorkspace} from '~/logic/state/metadata';
import { ConnectionStatus } from '~/types';

type GroupsPaneProps = {
  baseUrl: string;
  workspace: Workspace;
  api: GlobalApi;
  connection: ConnectionStatus;
};

export function GroupsPane(props: GroupsPaneProps) {
  const { baseUrl, api, workspace } = props;
  const groupAssociation = useAssocForWorkspace(workspace)
  const notificationsCount = useHarkState(state => state.notificationsCount);
  const group = useGroupForAssoc(groupAssociation)
  const relativePath = useCallback(p => `${baseUrl}${p}`, [baseUrl]);

  const [recentGroups, setRecentGroups] = useLocalStorageState<string[]>(
    'recent-groups',
    []
  );

  useEffect(() => {
    if (workspace.type !== 'group') {
      return;
    }
    setRecentGroups(gs => _.uniq([workspace.group, ...gs]));
  }, [workspace]);

  if (workspace.type === 'group' && !groupAssociation) {
    return null;
  }

  const popovers = (routeProps: RouteComponentProps, baseUrl: string) =>
     ( <>
     {groupAssociation && 
       ( <PopoverRoutes
          association={groupAssociation!}
          group={group!}
          api={api}
          baseUrl={baseUrl}
                        />)}
        <InvitePopover
          api={api}
          association={groupAssociation!}
          baseUrl={baseUrl}
          workspace={workspace}
        />
      </>
    );

  return (
    <Switch>
      <Route
        path={[relativePath('/resource/:app/(ship)?/:host/:name')]}
        render={(routeProps) => {
          const { app, host, name } = routeProps.match.params as Record<
            string,
            string
          >;

          const appName = app as AppName;

          const resource = `/ship/${host}/${name}`;
          const resourceUrl = `${baseUrl}/resource/${app}${resource}`;


          return (
            <Skeleton
              mobileHide
              recentGroups={recentGroups}
              selected={resource}
              selectedApp={appName}
              {...props}
              baseUrl={resourceUrl}
            >
              <Resource
                workspace={workspace}
                api={api}
                resource={resource}
                baseUrl={baseUrl}
              />
              {popovers(routeProps, resourceUrl)}
            </Skeleton>
          );
        }}
      />
      <Route
        path={relativePath('/join/:app/(ship)?/:host/:name')}
        render={(routeProps) => {
          const { app, host, name } = routeProps.match.params;
          const appPath = `/ship/${host}/${name}`;
          const resourceUrl = `${baseUrl}/join/${app}${appPath}`;
          let title = groupAssociation?.metadata?.title ?? 'Landscape';


          return (
            <>
              <Skeleton
                recentGroups={recentGroups}
                mobileHide
                selected={appPath}
                {...props}
                baseUrl={baseUrl}
              >
                <UnjoinedResource
                  baseUrl={baseUrl}
                  api={api}
                  resource={appPath}
                />
                {popovers(routeProps, resourceUrl)}
              </Skeleton>
            </>
          );
        }}
      />
      <Route
        path={relativePath('/new')}
        render={(routeProps) => {
          const newUrl = `${baseUrl}/new`;
          return (
            <Skeleton mobileHide recentGroups={recentGroups} {...props} baseUrl={baseUrl}>
              <NewChannel
                {...routeProps}
                api={api}
                baseUrl={baseUrl}
                group={groupPath}
                workspace={workspace}
              />
              {popovers(routeProps, baseUrl)}
            </Skeleton>
          );
        }}
      />
      <Route
        path={relativePath('')}
        render={(routeProps) => {
          const channelCount = Object.keys(associations?.graph ?? {}).filter((e) => {
            return associations?.graph?.[e]?.['group'] === groupPath;
          }).length;
          let summary: ReactNode;
          if(groupAssociation?.group) {
            const memberCount = groups[groupAssociation.group].members.size;
            summary = <GroupSummary
              memberCount={memberCount}
              channelCount={channelCount}
              metadata={groupAssociation.metadata}
              resource={groupAssociation.group}
                      />;
          } else {
            summary = (<Box p="4"><Text color='gray'>
                        Create or select a channel to get started
                      </Text></Box>);
          }
          const title = groupAssociation?.metadata?.title ?? 'Landscape';
          return (
            <>
              <Helmet defer={false}>
                <title>{notificationsCount ? `(${String(notificationsCount)}) ` : ''}{ title }</title>
              </Helmet>
              <Skeleton recentGroups={recentGroups} {...props} baseUrl={baseUrl}>
                <Col
                  alignItems="center"
                  justifyContent="center"
                  display={['none', 'flex']}
                  p='4'
                >
                {summary}
                </Col>
                {popovers(routeProps, baseUrl)}
              </Skeleton>
            </>
          );
        }}
      />
    </Switch>
  );
}
