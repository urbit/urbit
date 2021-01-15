import React, { useCallback } from "react";
import { Row, Box, Col } from "@tlon/indigo-react";
import styled from "styled-components";
import Helmet from 'react-helmet';

import { ChatResource } from "~/views/apps/chat/ChatResource";
import { PublishResource } from "~/views/apps/publish/PublishResource";
import { LinkResource } from "~/views/apps/links/LinkResource";

import { Association } from "~/types/metadata-update";
import { StoreState } from "~/logic/store/type";
import GlobalApi from "~/logic/api/global";
import { RouteComponentProps, Route, Switch } from "react-router-dom";
import { ChannelSettings } from "./ChannelSettings";
import { ResourceSkeleton } from "./ResourceSkeleton";

const TruncatedBox = styled(Box)`
  white-space: nowrap;
  text-overflow: ellipsis;
  overflow: hidden;
`;

type ResourceProps = StoreState & {
  association: Association;
  api: GlobalApi;
  baseUrl: string;
} & RouteComponentProps;

export function Resource(props: ResourceProps) {
  const { association, api, notificationsGraphConfig } = props;
  const app = association.metadata.module || association["app-name"];
  const rid = association.resource;
  const selectedGroup = association.group;
  const relativePath = (p: string) =>

    `${props.baseUrl}/resource/${app}${rid}${p}`;
  const skelProps = { api, association };
  let title = props.association.metadata.title;
  if ('workspace' in props) {
    if ('group' in props.workspace && props.workspace.group in props.associations.contacts) {
      title = `${props.associations.contacts[props.workspace.group].metadata.title} - ${props.association.metadata.title}`;
    }
  }
  return (
    <>
      <Helmet defer={false}>
        <title>{props.notificationsCount ? `(${String(props.notificationsCount)}) ` : ''}{ title }</title>
      </Helmet>
      <Switch>
        <Route
          path={relativePath("/settings")}
          render={(routeProps) => {
            return (
              <ResourceSkeleton
                baseUrl={props.baseUrl}
                {...skelProps}
              >
                <ChannelSettings
                  groups={props.groups}
                  contacts={props.contacts}
                  associations={props.associations}
                  api={api}
                  association={association}
                />
              </ResourceSkeleton>
            );
          }}
        />
        <Route
          path={relativePath("")}
          render={(routeProps) => (
            <ResourceSkeleton
              notificationsGraphConfig={props.notificationsGraphConfig}
              notificationsChatConfig={props.notificationsChatConfig}
              baseUrl={props.baseUrl}
              {...skelProps}
              atRoot
            >
              {app === "chat" ? (
                <ChatResource {...props} />
              ) : app === "publish" ? (
                <PublishResource {...props} />
              ) : (
                <LinkResource {...props} />
              )}
            </ResourceSkeleton>
          )}
        />
      </Switch>
    </>
  );
}
