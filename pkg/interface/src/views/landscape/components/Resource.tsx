import { Association } from '@urbit/api/metadata';
import React, { ReactElement } from 'react';
import Helmet from 'react-helmet';
import { Route, Switch } from 'react-router-dom';
import GlobalApi from '~/logic/api/global';
import useContactState from '~/logic/state/contact';
import useGroupState from '~/logic/state/group';
import useHarkState from '~/logic/state/hark';
import useMetadataState from '~/logic/state/metadata';
import { StoreState } from '~/logic/store/type';
import { Workspace } from '~/types';
import { ChatResource } from '~/views/apps/chat/ChatResource';
import { LinkResource } from '~/views/apps/links/LinkResource';
import { PublishResource } from '~/views/apps/publish/PublishResource';
import { ChannelPopoverRoutes } from './ChannelPopoverRoutes';
import { ResourceSkeleton } from './ResourceSkeleton';

type ResourceProps = StoreState & {
  association: Association;
  api: GlobalApi;
  baseUrl: string;
  workspace: Workspace;
};

export function Resource(props: ResourceProps): ReactElement {
  const { association, api } = props;
  const groups = useGroupState(state => state.groups);
  const notificationsCount = useHarkState(state => state.notificationsCount);
  const associations = useMetadataState(state => state.associations);
  const contacts = useContactState(state => state.contacts);
  let app = association['app-name'];
  if (association?.metadata?.config && 'graph' in association.metadata.config) {
    app = association.metadata.config.graph;
  }
  const { resource: rid, group: selectedGroup } = association;
  const relativePath = (p: string) => `${props.baseUrl}/resource/${app}${rid}${p}`;
  const skelProps = { api, association, groups, contacts };
  let title = props.association.metadata.title;
  if ('group' in props.workspace && props.workspace.group in associations.groups) {
      title = `${associations.groups[props.workspace.group].metadata.title} - ${props.association.metadata.title}`;
  }

  return (
    <>
      <Helmet defer={false}>
        <title>{notificationsCount ? `(${String(notificationsCount)}) ` : ''}{ title }</title>
      </Helmet>
      { app === 'link' ? (
        <LinkResource {...props} />
      ) :  (
      <ResourceSkeleton
        {...skelProps}
        baseUrl={relativePath('')}
      >
        {app === 'chat' ? (
          <ChatResource {...props} />
        ) : app === 'publish' ? (
          <PublishResource {...props} />
        ) : null }
      </ResourceSkeleton>
      )}
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
