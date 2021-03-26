import React, { ReactElement, useCallback } from 'react';
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
import useGroupState, {useGroupForAssoc} from '~/logic/state/group';
import useContactState from '~/logic/state/contact';
import useHarkState from '~/logic/state/hark';
import useMetadataState, {useAssocForGraph, useAssocForWorkspace} from '~/logic/state/metadata';
import {Workspace} from '~/types';

type ResourceProps = {
  resource: string;
  api: GlobalApi;
  baseUrl: string;
  workspace: Workspace;
};

export function Resource(props: ResourceProps): ReactElement {
  const { workspace, resource, api, baseUrl } = props;
  const association = useAssocForGraph(resource)!;
  const group = useGroupForAssoc(association)!;
  const groupAssociation = useAssocForWorkspace(workspace);
  const notificationsCount = useHarkState(state => state.notificationsCount);
  const contacts = useContactState(state => state.contacts);
  const app = association.metadata.module || association['app-name'];
  const rid = association.resource;
  const relativePath = useCallback(p => 
    `${baseUrl}/resource/${app}${rid}${p}`,
    [baseUrl, association]
  );
  const skelProps = { api, association, contacts };
  const resourceProps = { api, association, baseUrl };
  let title = association.metadata.title;
  if (groupAssociation) {
    title = `${groupAssociation.metadata.title} - ${title}`;
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
          <ChatResource {...resourceProps} />
        ) : app === 'publish' ? (
          <PublishResource {...resourceProps} />
        ) : (
          <LinkResource {...resourceProps} />
        )}
      </ResourceSkeleton>
      <Switch>
        <Route
          path={relativePath('/settings')}
          render={(routeProps) => {
            return (
              <ChannelPopoverRoutes
                association={association}
                group={group}
                api={api}
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

Resource.whyDidYouRender = true;
