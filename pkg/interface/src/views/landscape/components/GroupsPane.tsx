import React, { useState, useEffect } from "react";
import {
  Switch,
  Route,
  useLocation,
  RouteComponentProps,
} from "react-router-dom";
import { Col, Box, Text } from "@tlon/indigo-react";
import _ from "lodash";

import { Resource } from "./Resource";
import { PopoverRoutes } from "./PopoverRoutes";
import { Skeleton } from "./Skeleton";
import { InvitePopover } from "./InvitePopover";
import { NewChannel } from "./NewChannel";

import { appIsGraph } from "~/logic/lib/util";
import { AppName } from "~/types/noun";
import GlobalApi from "~/logic/api/global";
import { StoreState } from "~/logic/store/type";
import { UnjoinedResource } from "~/views/components/UnjoinedResource";
import { useLocalStorageState } from "~/logic/lib/useLocalStorageState";
import { Loading } from '~/views/components/Loading';

import "~/views/apps/links/css/custom.css";
import "~/views/apps/publish/css/custom.css";
import { Workspace } from "~/types";
import { getGroupFromWorkspace } from "~/logic/lib/workspace";

type GroupsPaneProps = StoreState & {
  baseUrl: string;
  workspace: Workspace;
  api: GlobalApi;
};

export function GroupsPane(props: GroupsPaneProps) {
  const { baseUrl, associations, groups, contacts, api, workspace } = props;
  const relativePath = (path: string) => baseUrl + path;
  const groupPath = getGroupFromWorkspace(workspace);

  const groupContacts = (groupPath && contacts[groupPath]) || undefined;
  const rootIdentity = contacts?.["/~/default"]?.[window.ship];
  const groupAssociation =
    (groupPath && associations.contacts[groupPath]) || undefined;
  const group = (groupPath && groups[groupPath]) || undefined;
  const [recentGroups, setRecentGroups] = useLocalStorageState<string[]>(
    "recent-groups",
    []
  );

  useEffect(() => {
    if (workspace.type !== "group") {
      return;
    }
    setRecentGroups((gs) => _.uniq([workspace.group, ...gs]));
  }, [workspace]);

  if (!associations) {
    return null;
  }

  const popovers = (routeProps: RouteComponentProps, baseUrl: string) =>
     ( <>
        {groupPath && ( <PopoverRoutes
          contacts={groupContacts || {}}
          rootIdentity={rootIdentity}
          association={groupAssociation!}
          group={group!}
          api={api}
          s3={props.s3}
          hideAvatars={props.hideAvatars}
          hideNicknames={props.hideNicknames}
          notificationsGroupConfig={props.notificationsGroupConfig}

          {...routeProps}
          baseUrl={baseUrl}
        />)}
        <InvitePopover
          api={api}
          association={groupAssociation!}
          baseUrl={baseUrl}
          groups={props.groups}
          contacts={props.contacts}
          workspace={workspace}
        />
      </>
    )

  return (
    <Switch>
      <Route
        path={[relativePath("/resource/:app/(ship)?/:host/:name")]}
        render={(routeProps) => {
          const { app, host, name } = routeProps.match.params as Record<
            string,
            string
          >;
          const appName = app as AppName;
          const isGraph = appIsGraph(app);

          const resource = `${isGraph ? "/ship" : ""}/${host}/${name}`;
          const association =
            isGraph
              ? associations.graph[resource]
              : associations[appName][resource];
          const resourceUrl = `${baseUrl}/resource/${app}${resource}`;

          if (!association) {
            return <Loading />;
          }

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
                {...props}
                {...routeProps}
                association={association}
                baseUrl={baseUrl}
              />
              {popovers(routeProps, resourceUrl)}
            </Skeleton>
          );
        }}
      />
      <Route
        path={relativePath("/join/:app/(ship)?/:host/:name")}
        render={(routeProps) => {
          const { app, host, name } = routeProps.match.params;
          const appName = app as AppName;
          const isGraph = appIsGraph(app);
          const appPath = `${isGraph ? '/ship/' : '/'}${host}/${name}`;
          const association = isGraph ? associations.graph[appPath] : associations[appName][appPath];
          const resourceUrl = `${baseUrl}/join/${app}${appPath}`;

          if (!association) {
            return <Loading />;
          }
          return (
            <Skeleton
              recentGroups={recentGroups}
              mobileHide
              selected={appPath}
              {...props}
              baseUrl={baseUrl}
            >
              <UnjoinedResource
                graphKeys={props.graphKeys}
                notebooks={props.notebooks}
                inbox={props.inbox}
                baseUrl={baseUrl}
                api={api}
                association={association}
              />
              {popovers(routeProps, resourceUrl)}
            </Skeleton>
          );
        }}
      />
      <Route
        path={relativePath("/new")}
        render={(routeProps) => {
          const newUrl = `${baseUrl}/new`;
          return (
            <Skeleton mobileHide recentGroups={recentGroups} {...props} baseUrl={baseUrl}>
              <NewChannel
                {...routeProps}
                api={api}
                baseUrl={baseUrl}
                chatSynced={props.chatSynced}
                associations={associations}
                groups={groups}
                group={groupPath}
                contacts={props.contacts}
                workspace={workspace}
              />
              {popovers(routeProps, baseUrl)}
            </Skeleton>
          );
        }}
      />
      <Route
        path={relativePath("")}
        render={(routeProps) => {
          const hasDescription = groupAssociation?.metadata?.description;
          const description = (hasDescription && hasDescription !== "")
            ? hasDescription : "Create or select a channel to get started"
          return (
            <Skeleton recentGroups={recentGroups} {...props} baseUrl={baseUrl}>
              <Col
                alignItems="center"
                justifyContent="center"
                display={["none", "flex"]}
                p='4'
              >
                <Box p="4"><Text fontSize="0" color='gray'>
                  {description}
                </Text></Box>
              </Col>
              {popovers(routeProps, baseUrl)}
            </Skeleton>
          );
        }}
      />
    </Switch>
  );
}
