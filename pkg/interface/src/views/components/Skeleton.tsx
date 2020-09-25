import React, { ReactNode, useEffect } from "react";
import { Box, Text } from "@tlon/indigo-react";
import { Link } from "react-router-dom";

import { Sidebar } from "./Sidebar";
import { ChatHookUpdate } from "~/types/chat-hook-update";
import { Inbox } from "~/types/chat-update";
import { Associations } from "~/types/metadata-update";
import { Notebooks } from "~/types/publish-update";
import GlobalApi from "~/logic/api/global";
import { Path, AppName } from "~/types/noun";
import { LinkCollections } from "~/types/link-update";
import styled from "styled-components";
import GlobalSubscription from "~/logic/subscription/global";
import {Workspace} from "~/types";

interface SkeletonProps {
  children: ReactNode;
  recentGroups: string[];
  associations: Associations;
  chatSynced: ChatHookUpdate | null;
  graphKeys: Set<string>;
  linkListening: Set<Path>;
  links: LinkCollections;
  notebooks: Notebooks;
  inbox: Inbox;
  selected?: string;
  selectedApp?: AppName;
  baseUrl: string;
  mobileHide?: boolean;
  api: GlobalApi;
  subscription: GlobalSubscription;
  includeUnmanaged: boolean;
  workspace: Workspace;
}

export function Skeleton(props: SkeletonProps) {
  const chatConfig = {
    name: "chat",
    getStatus: (s: string) => {
      if (!(s in (props.chatSynced || {}))) {
        return "unsubscribed";
      }
      const mailbox = props?.inbox?.[s];
      if(!mailbox) {
        return undefined;
      }
      const { config } = mailbox;
      if (config?.read !== config?.length) {
        return "unread";
      }
      return undefined;
    },
  };
  const publishConfig = {
    name: "chat",
    getStatus: (s: string) => {
      const [, host, name] = s.split("/");
      const notebook = props.notebooks?.[host]?.[name];
      if (!notebook) {
        return "unsubscribed";
      }
      if (notebook["num-unread"]) {
        return "unread";
      }
      return undefined;
    },
  };
  const linkConfig = {
    name: "link",
    getStatus: (s: string) => {
      const [, , host, name] = s.split("/");
      const graphKey = `${host.slice(1)}/${name}`;

      if (!props.graphKeys.has(graphKey)) {
        return "unsubscribed";
      }
      const link = props.links[s];
      if (!link) {
        return undefined;
      }
      if (link.unseenCount > 0) {
        return "unread";
      }
      return undefined;
    },
  };
  const config = {
    publish: publishConfig,
    link: linkConfig,
    chat: chatConfig,
  };

  useEffect(() => {
    props.api.publish.fetchNotebooks();
    props.subscription.startApp("chat");
    props.subscription.startApp("publish");
    props.subscription.startApp("graph");
  }, []);

  return (
    <Box fontSize={0} px={[0, 3]} pb={[0, 3]} height="100%" width="100%">
      <Box
        bg="white"
        height="100%"
        width="100%"
        display="grid"
        borderRadius={1}
        border={[0, 1]}
        borderColor={["washedGray", "washedGray"]}
        gridTemplateColumns={["1fr", "250px 1fr"]}
        gridTemplateRows="1fr"
      >
        <Sidebar
          recentGroups={props.recentGroups}
          selected={props.selected}
          selectedApp={props.selectedApp}
          associations={props.associations}
          invites={{}}
          apps={config}
          baseUrl={props.baseUrl}
          includeUnmanaged={!props.selectedGroup}
          mobileHide={props.mobileHide}
          workspace={props.workspace}
        ></Sidebar>
        {props.children}
      </Box>
    </Box>
  );
}
