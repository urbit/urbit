import React, { ReactNode, useEffect, useMemo } from "react";
import { Box, Text } from "@tlon/indigo-react";
import { Link } from "react-router-dom";

import { Sidebar } from "./Sidebar/Sidebar";
import { ChatHookUpdate } from "~/types/chat-hook-update";
import { Inbox } from "~/types/chat-update";
import { Associations } from "~/types/metadata-update";
import { Notebooks } from "~/types/publish-update";
import GlobalApi from "~/logic/api/global";
import { Path, AppName } from "~/types/noun";
import { LinkCollections } from "~/types/link-update";
import styled from "styled-components";
import GlobalSubscription from "~/logic/subscription/global";
import { Workspace, Groups, Graphs, Invites } from "~/types";
import { useChat, usePublish, useLinks } from "./Sidebar/Apps";
import { Body } from "~/views/components/Body";

interface SkeletonProps {
  children: ReactNode;
  recentGroups: string[];
  groups: Groups;
  associations: Associations;
  chatSynced: ChatHookUpdate | null;
  graphKeys: Set<string>;
  graphs: Graphs;
  linkListening: Set<Path>;
  links: LinkCollections;
  notebooks: Notebooks;
  invites: Invites;
  inbox: Inbox;
  selected?: string;
  selectedApp?: AppName;
  baseUrl: string;
  mobileHide?: boolean;
  api: GlobalApi;
  subscription: GlobalSubscription;
  includeUnmanaged: boolean;
  workspace: Workspace;
  hideSidebar?: boolean;
}

export function Skeleton(props: SkeletonProps) {
  const chatConfig = useChat(props.inbox, props.chatSynced);
  const publishConfig = usePublish(props.notebooks);
  const linkConfig = useLinks(props.graphKeys, props.graphs);
  const config = useMemo(
    () => ({
      publish: publishConfig,
      link: linkConfig,
      chat: chatConfig,
    }),
    [publishConfig, linkConfig, chatConfig]
  );

  return (
    <Body
      display="grid"
      gridTemplateColumns={["100%", "250px 1fr"]}
      gridTemplateRows="100%"
    >
      {!props.hideSidebar && (
        <Sidebar
          api={props.api}
          recentGroups={props.recentGroups}
          selected={props.selected}
          associations={props.associations}
          invites={props.invites}
          apps={config}
          baseUrl={props.baseUrl}
          groups={props.groups}
          mobileHide={props.mobileHide}
          workspace={props.workspace}
        ></Sidebar>
      )}
      {props.children}
    </Body>
  );
}
