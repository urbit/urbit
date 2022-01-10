import { readGroup } from '@urbit/api';
import _ from 'lodash';
import React, { useCallback, useEffect } from 'react';
import Helmet from 'react-helmet';
import { Box } from '@tlon/indigo-react';
import {
  Route,
  RouteComponentProps, Switch
} from 'react-router-dom';
import { useShortcut } from '~/logic/state/settings';
import { useLocalStorageState } from '~/logic/lib/useLocalStorageState';
import { getGroupFromWorkspace } from '~/logic/lib/workspace';
import useGroupState from '~/logic/state/group';
import useHarkState from '~/logic/state/hark';
import useMetadataState from '~/logic/state/metadata';
import { DmResource } from '~/views/apps/chat/DmResource';
import { Workspace } from '~/types/workspace';
import '~/views/apps/links/css/custom.css';
import '~/views/apps/publish/css/custom.css';
import { Loading } from '~/views/components/Loading';
import { UnjoinedResource } from '~/views/components/UnjoinedResource';
import { EmptyGroupHome } from './Home/EmptyGroupHome';
import { GroupHome } from './Home/GroupHome';
import { InvitePopover } from './InvitePopover';
import { NewChannel } from './NewChannel';
import { PopoverRoutes } from './PopoverRoutes';
import { Resource } from './Resource';
import { Skeleton } from './Skeleton';
import { Join } from './Join/Join';
import { UqbarHome } from './Home/UqbarHome';

interface GroupsPaneProps {
  baseUrl: string;
  workspace: Workspace;
  isHome?: boolean;
}

export function GroupsPane(props: GroupsPaneProps) {
  const { baseUrl, workspace, isHome = false } = props;
  const associations = useMetadataState(state => state.associations);
  const notificationsCount = useHarkState(state => state.notificationsCount);

  const relativePath = (path: string) => baseUrl + path;
  const groupPath = getGroupFromWorkspace(workspace);
  const groups = useGroupState(state => state.groups);

  useShortcut('readGroup', useCallback(() => {
    if(groupPath) {
      useHarkState.getState().readGroup(groupPath);
    }
  }, [groupPath]));

  const groupAssociation =
    (groupPath && associations.groups[groupPath]) || undefined;
  const group = (groupPath && groups[groupPath]) || undefined;
  const [recentGroups, setRecentGroups] = useLocalStorageState<string[]>(
    'recent-groups',
    []
  );

  useEffect(() => {
    if (workspace.type !== 'group') {
      return;
    }
    const { pendingJoin, doneJoin } = useGroupState.getState();
    const group = getGroupFromWorkspace(workspace)!;
    if(group in pendingJoin) {
      doneJoin(group);
    }

    return () => {
      setRecentGroups(gs => _.uniq([workspace.group, ...gs]));
    };
  }, [workspace]);

  if (isHome) {
    return (
      <>
        <Helmet defer={false}>
          <title>Uqbar UI</title>
        </Helmet>
        <Skeleton
          {...props}
          mobileHide={false}
          recentGroups={recentGroups}
          baseUrl={baseUrl}
        >
          <UqbarHome />
        </Skeleton>
      </>
    );
  }

  if (!(associations && (groupPath ? groupPath in groups : true))) {
    return null;
  }

  const popovers = (routeProps: RouteComponentProps, baseUrl: string) =>
     ( <>
        {groupPath && ( <PopoverRoutes
          association={groupAssociation!}
          group={group!}

          {...routeProps}
          baseUrl={baseUrl}
                        />)}
        <InvitePopover
          association={groupAssociation!}
          baseUrl={baseUrl}
          workspace={workspace}
        />
      </>
    );

  return (
    <Switch>
      <Route
        path={relativePath('/dm/:ship')}
        render={({ match }) => {
          const { ship } = match.params as Record<string, string>;

          return (
            <Skeleton
              mobileHide
              recentGroups={recentGroups}
              selected={ship}
              {...props}
              baseUrl={match.path}
            > <DmResource ship={ship} />

            </Skeleton>

          );
        }}
      />
      <Route
        path={[relativePath('/resource/:app/(ship)?/:host/:name')]}
        render={(routeProps) => {
          const { app, host, name } = routeProps.match.params as Record<
            string,
            string
          >;

          const resource = `/ship/${host}/${name}`;
          const association = associations.graph[resource];
          const resourceUrl = `${baseUrl}/resource/${app}${resource}`;

          if (!association) {
            return <Loading />;
          }

          return (
            <Skeleton
              mobileHide
              recentGroups={recentGroups}
              selected={resource}
              {...props}
              baseUrl={resourceUrl}
            >
              <Resource
                workspace={props.workspace}
                association={association}
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
          const association = associations.graph[appPath];
          const resourceUrl = `${baseUrl}/join/${app}${appPath}`;
          let title = groupAssociation?.metadata?.title ?? 'Groups';

          if (!association) {
            return <Loading />;
          }

          title += ` - ${association.metadata.title}`;
          return (
            <>
              <Helmet defer={false}>
                <title>{notificationsCount ? `(${String(notificationsCount)}) ` : ''}{ title }</title>
              </Helmet>
              <Skeleton
                recentGroups={recentGroups}
                mobileHide
                selected={appPath}
                {...props}
                baseUrl={baseUrl}
              >
                <UnjoinedResource
                  baseUrl={baseUrl}
                  association={association}
                />
                {popovers(routeProps, resourceUrl)}
              </Skeleton>
            </>
          );
        }}
    />
    <Route
      path={relativePath('/pending/:ship/:name')}
      render={(routeProps) => {
        const { ship, name } = routeProps.match.params as Record<string, string>;
        const desc =  {
          group: `/ship/${ship}/${name}`,
          kind: 'graph' as const
        };
       return (<Skeleton
        mobileHide
        recentGroups={recentGroups}
        {...props}
        baseUrl={baseUrl}
      >
        <Box width="100%">
          <Join desc={desc} />
        </Box>
      </Skeleton>
       );
      }}
    >
    </Route>
      <Route
        path={relativePath('/new')}
        render={(routeProps) => {
          return (
            <Skeleton mobileHide recentGroups={recentGroups} {...props} baseUrl={baseUrl}>
              <NewChannel
                {...routeProps}
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
        path={[relativePath('/'), relativePath('/feed+')]}
        render={(routeProps) => {
          const shouldHideSidebar =
            routeProps.location.pathname.includes('/feed');
          const title = groupAssociation?.metadata?.title ?? 'Groups';
          return (
            <>
              <Helmet defer={false}>
                <title>
                  {notificationsCount ? `(${String(notificationsCount)}) ` : ''}
                  { title }
                </title>
              </Helmet>
              <Skeleton
                {...props}
                mobileHide={shouldHideSidebar}
                recentGroups={recentGroups}
                baseUrl={baseUrl}
              >
                { workspace.type === 'group' ? (
                  <GroupHome
                    baseUrl={baseUrl}
                    groupPath={groupPath}
                  />
                  ) : (
                    <EmptyGroupHome
                      associations={associations}
                    />
                )}
               {popovers(routeProps, baseUrl)}
              </Skeleton>
            </>
          );
        }}
      />
    </Switch>
  );
}
