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
interface SkeletonProps {
  children: ReactNode;
  recentGroups: string[];
  associations: Associations;
  chatSynced: ChatHookUpdate | null;
  linkListening: Set<Path>;
  links: LinkCollections;
  notebooks: Notebooks;
  inbox: Inbox;
  selected?: string;
  selectedApp?: AppName;
  selectedGroup: string;
  baseUrl: string;
  mobileHide?: boolean;
  api: GlobalApi;
}

const buntAppConfig = (name: string) => ({
  name,
  getStatus: (s: string) => undefined,
});

const TruncatedBox = styled(Box)`
  white-space: nowrap;
  text-overflow: ellipsis;
  overflow: hidden;
`;

export function Skeleton(props: SkeletonProps) {
  const chatConfig = {
    name: "chat",
    getStatus: (s: string) => {
      if (!(s in (props.chatSynced || {}))) {
        return "unsubscribed";
      }
      const { config } = props.inbox[s];
      if (config.read !== config.length) {
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
      if (!props.linkListening.has(s)) {
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
    props.subscription.startApp("link");
    props.api.links.getPage("", 0);
  }, []);

  const association =
    props.selected && props.selectedApp
      ? props.associations?.[props.selectedApp]?.[props.selected]
      : null;

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
        gridTemplateRows="32px 1fr"
      >
        <Sidebar
          recentGroups={props.recentGroups}
          selected={props.selected}
          selectedGroup={props.selectedGroup}
          selectedApp={props.selectedApp}
          associations={props.associations}
          invites={{}}
          apps={config}
          baseUrl={props.baseUrl}
          onlyGroups={[props.selectedGroup]}
          mobileHide={props.mobileHide}
        ></Sidebar>
        <Box
          p={2}
          display="flex"
          alignItems="center"
          borderBottom={1}
          borderBottomColor="washedGray"
        >
          <Box
            borderRight={1}
            borderRightColor="gray"
            pr={2}
            mr={2}
            display={["block", "none"]}
          >
            <Link to={`/~groups${props.selectedGroup}`}> {"<- Back"}</Link>
          </Box>
          <Box mr={2}>{association?.metadata?.title}</Box>
          <TruncatedBox
            maxWidth="50%"
            flexShrink={1}
            color="gray"
          >
            {association?.metadata?.description}
          </TruncatedBox>
        </Box>
        {props.children}
      </Box>
    </Box>
  );
}
