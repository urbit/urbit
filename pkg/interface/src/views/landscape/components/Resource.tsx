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
import {ChannelPopoverRoutes} from "./ChannelPopoverRoutes";

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
  const { association, api, notificationsGraphConfig, groups, contacts } = props;
  const app = association.metadata.module || association["app-name"];
  const rid = association.resource;
  const selectedGroup = association.group;
  const relativePath = (p: string) =>
    `${props.baseUrl}/resource/${app}${rid}${p}`;
  const skelProps = { api, association, groups, contacts };
  let title = props.association.metadata.title;
  if ('workspace' in props) {
    if ('group' in props.workspace && props.workspace.group in props.associations.groups) {
      title = `${props.associations.groups[props.workspace.group].metadata.title} - ${props.association.metadata.title}`;
    }
  }
  return (
    <>
      <Helmet defer={false}>
        <title>{props.notificationsCount ? `(${String(props.notificationsCount)}) ` : ''}{ title }</title>
      </Helmet>
      <ResourceSkeleton
        {...skelProps}
        baseUrl={relativePath("")}
      >
        {app === "chat" ? (
          <ChatResource {...props} />
        ) : app === "publish" ? (
          <PublishResource {...props} />
        ) : (
          <LinkResource {...props} />
        )}
      </ResourceSkeleton>
      <Switch>
        <Route
          path={relativePath("/settings")}
          render={(routeProps) => {
            return (
              <ChannelPopoverRoutes
                association={association}
                group={props.groups?.[selectedGroup]}
                groups={props.groups}
                contacts={props.contacts}
                api={props.api}
                baseUrl={relativePath("")}
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
