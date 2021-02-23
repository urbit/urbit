import React, { ReactElement } from 'react';
import Helmet from 'react-helmet';
import { RouteComponentProps, Route, Switch } from 'react-router-dom';

import { Association, NotificationGraphConfig } from '@urbit/api';

import { ChatResource } from '~/views/apps/chat/ChatResource';
import { PublishResource } from '~/views/apps/publish/PublishResource';
import { LinkResource } from '~/views/apps/links/LinkResource';
import { StoreState } from '~/logic/store/type';
import { ResourceSkeleton } from './ResourceSkeleton';
import { ChannelPopoverRoutes } from './ChannelPopoverRoutes';
import { Workspace } from '~/types';
import useMetadataState from '~/logic/state/metadata';
import useHarkState from '~/logic/state/hark';
import useGroupState from '~/logic/state/groups';

type ResourceProps = StoreState & {
  association: Association;
  baseUrl: string;
  workspace: Workspace;
  notificationsGraphConfig: NotificationGraphConfig;
} & RouteComponentProps;

export function Resource(props: ResourceProps): ReactElement {
  const { association, notificationsGraphConfig } = props;
  const associations = useMetadataState(state => state.associations);
  const notificationsCount = useHarkState(state => state.notificationsCount);
  const groups = useGroupState(state => state.groups);
  const app = association.metadata.module || association['app-name'];
  const rid = association.resource;
  const selectedGroup = association.group;
  const relativePath = (p: string) =>
    `${props.baseUrl}/resource/${app}${rid}${p}`;
  const skelProps = { association };
  let title = props.association.metadata.title;
  if ('workspace' in props) {
    if ('group' in props.workspace && props.workspace.group in associations.groups) {
      title = `${associations.groups[props.workspace.group].metadata.title} - ${props.association.metadata.title}`;
    }
  }
  return (
    <>
      <Helmet defer={false}>
        <title>{notificationsCount ? `(${String(notificationsCount)}) ` : ''}{ title }</title>
      </Helmet>
      <ResourceSkeleton
        {...skelProps}
        baseUrl={relativePath('')}
      >
        {app === 'chat' ? (
          <ChatResource {...props} />
        ) : app === 'publish' ? (
          <PublishResource {...props} />
        ) : (
          <LinkResource {...props} />
        )}
      </ResourceSkeleton>
      <Switch>
        <Route
          path={relativePath('/settings')}
          render={(routeProps) => {
            return (
              <ChannelPopoverRoutes
                association={association}
                group={groups?.[selectedGroup]}
                baseUrl={relativePath('')}
                rootUrl={props.baseUrl}
                notificationsGraphConfig={notificationsGraphConfig}
              />
            );
          }}
        />
      </Switch>
    </>
  );
}
