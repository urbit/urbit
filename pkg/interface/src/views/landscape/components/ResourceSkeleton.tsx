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
  white-space: pre;
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
  groupTags?: any;
};

export function ResourceSkeleton(props: ResourceSkeletonProps) {
  const { association, api, baseUrl, children, atRoot, groupTags } = props;
  const app = association?.metadata?.module || association["app-name"];
  const rid = association.resource; 
  const workspace =
    baseUrl === "/~landscape/home" ? "/home" : association.group;
  const title = props.title || association?.metadata?.title;

  const [, , ship, resource] = rid.split("/");

  const resourcePath = (p: string) => baseUrl + `/resource/${app}/ship/${ship}/${resource}` + p;

  const isOwn = `~${window.ship}` === ship;
  let isWriter = (app === 'publish') ? true : false;

  if (groupTags?.publish?.[`writers-${resource}`]) {
    isWriter = isOwn || groupTags?.publish?.[`writers-${resource}`]?.has(window.ship);
  }

  return (
    <Col width="100%" height="100%" overflowY="hidden">
      <Box
        flexShrink="0"
        height='48px'
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
            fontSize='1'
            mr={3}
            my="1"
            display={["block", "none"]}
            flexShrink={0}
          >
            <Link to={`/~landscape${workspace}`}> {"<- Back"}</Link>
          </Box>
        ) : (
          <Box color="blue" pr={2} mr={2}>
            <Link to={`/~landscape${workspace}/resource/${app}${rid}`}>
              <Text color="blue">Go back to channel</Text>
            </Link>
          </Box>
        )}

        {atRoot && (
          <>
            <Box px={1} mr={2} minWidth={0} display="flex">
              <Text fontSize='2' fontWeight='700' display="inline-block" verticalAlign="middle" textOverflow="ellipsis" overflow="hidden" whiteSpace="pre" minWidth={0}>
                {title}
              </Text>
            </Box>
            <TruncatedBox
              display={["none", "block"]}
              verticalAlign="middle"
              maxWidth='60%'
              flexShrink={1}
              title={association?.metadata?.description}
              color="gray"
            >
              <RichText
                color="gray"
                mb="0"
                display="inline-block"
                disableRemoteContent
              >
                {association?.metadata?.description}
              </RichText>
            </TruncatedBox>
            <Box flexGrow={1} />
            {isWriter && (
              <Link to={resourcePath('/new')} style={{ flexShrink: '0' }}>
                <Text bold pr='3' color='blue'>+ New Post</Text>
              </Link>
            )}
            <ChannelMenu
              graphNotificationConfig={props.notificationsGraphConfig}
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
