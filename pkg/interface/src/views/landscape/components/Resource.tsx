import React, { ReactElement } from 'react';
import Helmet from 'react-helmet';
import { RouteComponentProps, Route, Switch } from 'react-router-dom';

import { Association } from '@urbit/api/metadata';

import { ChatResource } from '~/views/apps/chat/ChatResource';
import { PublishResource } from '~/views/apps/publish/PublishResource';
import { LinkResource } from '~/views/apps/links/LinkResource';
import { StoreState } from '~/logic/store/type';
import GlobalApi from '~/logic/api/global';
import { ResourceSkeleton } from './ResourceSkeleton';
import { ChannelPopoverRoutes } from './ChannelPopoverRoutes';
import useGroupState from '~/logic/state/group';
import useContactState from '~/logic/state/contact';
import useHarkState from '~/logic/state/hark';
import useMetadataState from '~/logic/state/metadata';

type ResourceProps = StoreState & {
  association: Association;
  api: GlobalApi;
  baseUrl: string;
} & RouteComponentProps;

export function Resource(props: ResourceProps): ReactElement {
  const { association, api, notificationsGraphConfig } = props;
  const groups = useGroupState(state => state.groups);
  const notificationsCount = useHarkState(state => state.notificationsCount);
  const associations = useMetadataState(state => state.associations);
  const contacts = useContactState(state => state.contacts);
  const app = association.metadata.module || association['app-name'];
  const rid = association.resource;
  const selectedGroup = association.group;
  const relativePath = (p: string) =>
    `${props.baseUrl}/resource/${app}${rid}${p}`;
  const skelProps = { api, association, groups, contacts };
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
                api={props.api}
                baseUrl={relativePath('')}
                rootUrl={props.baseUrl}
              />
            );
          }}
        />
      </Switch>
    </>
  );
}
