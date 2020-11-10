import React, { ReactNode } from "react";
import { Row, Box, Col, Text } from "@tlon/indigo-react";
import styled from "styled-components";
import { Link } from "react-router-dom";

import { ChatResource } from "~/views/apps/chat/ChatResource";
import { PublishResource } from "~/views/apps/publish/PublishResource";

import RichText from "~/views/components/RichText";

import { Association } from "~/types/metadata-update";
import GlobalApi from "~/logic/api/global";
import { RouteComponentProps, Route, Switch } from "react-router-dom";
import { ChannelSettings } from "./ChannelSettings";
import { ChannelMenu } from "./ChannelMenu";
import { NotificationGraphConfig } from "~/types";

const TruncatedBox = styled(Box)`
  white-space: nowrap;
  text-overflow: ellipsis;
  overflow: hidden;
`;

type ResourceSkeletonProps = {
  association: Association;
  notificationsGraphConfig: NotificationGraphConfig;
  api: GlobalApi;
  baseUrl: string;
  children: ReactNode;
  atRoot?: boolean;
  title?: string;
};

export function ResourceSkeleton(props: ResourceSkeletonProps) {
  const { association, api, baseUrl, children, atRoot } = props;
  const app = association?.metadata?.module || association["app-name"];
  const appPath = association["app-path"];
  const workspace =
    baseUrl === "/~landscape/home" ? "/home" : association["group-path"];
  const title = props.title || association?.metadata?.title;
  const disableRemoteContent = {
    audioShown: false,
    imageShown: false,
    oembedShown: false,
    videoShown: false,
  };
  return (
    <Col width="100%" height="100%" overflowY="hidden">
      <Box
        flexShrink="0"
        py="2"
        px="2"
        display="flex"
        alignItems="center"
        borderBottom={1}
        borderBottomColor="washedGray"
      >
        {atRoot ? (
          <Box
            borderRight={1}
            borderRightColor="gray"
            pr={3}
            mr={3}
            my="1"
            display={["block", "none"]}
          >
            <Link to={`/~landscape${workspace}`}> {"<- Back"}</Link>
          </Box>
        ) : (
          <Box color="blue" pr={2} mr={2}>
            <Link to={`/~landscape${workspace}/resource/${app}${appPath}`}>
              <Text color="blue">Go back to channel</Text>
            </Link>
          </Box>
        )}

        {atRoot && (
          <>
            <Box pr={1} mr={2}>
              <Text display="inline-block" verticalAlign="middle">
                {title}
              </Text>
            </Box>
            <TruncatedBox
              display={["none", "block"]}
              maxWidth="60%"
              verticalAlign="middle"
              flexShrink={1}
              title={association?.metadata?.description}
              color="gray"
            >
              <RichText
                color="gray"
                remoteContentPolicy={disableRemoteContent}
                mb="0"
                display="inline-block"
              >
                {association?.metadata?.description}
              </RichText>
            </TruncatedBox>
            <Box flexGrow={1} />
            <ChannelMenu
              graphNotificationConfig={props.notificationsGraphConfig}
              chatNotificationConfig={props.notificationsChatConfig}
              association={association}
              api={api}
            />
          </>
        )}
      </Box>
      {children}
    </Col>
  );
}
