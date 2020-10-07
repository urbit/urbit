import React, { useCallback } from "react";
import { Row, Box, Col } from "@tlon/indigo-react";
import styled from "styled-components";
import { Link } from "react-router-dom";

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
  const { association, api } = props;
  const app = association.metadata.module || association["app-name"];
  const appPath = association["app-path"];
  const selectedGroup = association["group-path"];
  const relativePath = (p: string) =>
    `${props.baseUrl}/resource/${app}${appPath}${p}`;
  const skelProps = { api, association };
  return (
    <Switch>
      <Route
        path={relativePath("/settings")}
        render={(routeProps) => {
          return (
            <ResourceSkeleton
              baseUrl={props.baseUrl}
              {...skelProps}
            >
              <ChannelSettings api={api} association={association} />
            </ResourceSkeleton>
          );
        }}
      />
      <Route
        path={relativePath("")}
        render={(routeProps) => (
          <ResourceSkeleton baseUrl={props.baseUrl} {...skelProps} atRoot>
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
  );
}
