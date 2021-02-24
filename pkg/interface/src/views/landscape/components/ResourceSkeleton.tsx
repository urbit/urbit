import React, { ReactNode } from "react";
import { Row, Icon, Box, Col, Text } from "@tlon/indigo-react";
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
import { NotificationGraphConfig, Groups } from "~/types";
import {isWriter} from "~/logic/lib/group";
import urbitOb from 'urbit-ob';
import { getItemTitle } from '~/logic/lib/util';

const TruncatedBox = styled(Box)`
  white-space: pre;
  text-overflow: ellipsis;
  overflow: hidden;
`;

type ResourceSkeletonProps = {
  groups: Groups;
  contacts: any;
  association: Association;
  api: GlobalApi;
  baseUrl: string;
  children: ReactNode;
  title?: string;
  groupTags?: any;
};

export function ResourceSkeleton(props: ResourceSkeletonProps) {
  const { association, api, baseUrl, children, atRoot, groups } = props;
  const app = association?.metadata?.module || association["app-name"];
  const rid = association.resource;
  const group = groups[association.group];
  let workspace = association.group;

  if (group?.hidden && app === "chat") {
    workspace = "/messages";
  } else if (group?.hidden) {
    workspace = "/home";
  }

  let title = (workspace === "/messages")
    ? getItemTitle(association)
    : association?.metadata?.title;

  let recipient = false;

  if (urbitOb.isValidPatp(title)) {
    recipient = title;
    title = (props.contacts?.[title]?.nickname) ? props.contacts[title].nickname : title;
  }

  const [, , ship, resource] = rid.split("/");

  const resourcePath = (p: string) => baseUrl + p;

  const isOwn = `~${window.ship}` === ship;
  let canWrite = (app === 'publish') ? true : false;

  if (!isWriter(group, association.resource)) {
    canWrite = isOwn;
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
        <Box px={1} mr={2} minWidth={0} display="flex">
          <Text
            mono={urbitOb.isValidPatp(title)}
            fontSize='2'
            fontWeight='700'
            display="inline-block"
            verticalAlign="middle"
            textOverflow="ellipsis"
            overflow="hidden"
            whiteSpace="pre"
            minWidth={0}>
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
            display={(workspace === '/messages' && (urbitOb.isValidPatp(title))) ? "none" : "inline-block"}
            mono={(workspace === '/messages' && !(urbitOb.isValidPatp(title)))}
            color="gray"
            mb="0"
            disableRemoteContent
          >
            {(workspace === "/messages") ? recipient : association?.metadata?.description}
          </RichText>
        </TruncatedBox>
        <Box flexGrow={1} />
        {canWrite && (
          <Link to={resourcePath('/new')} style={{ flexShrink: '0' }}>
            <Text bold pr='3' color='blue'>+ New Post</Text>
          </Link>
      )}
      <Link to={`${baseUrl}/settings`}>
        <Icon icon="Menu" color="gray" pr="2" />
      </Link>
      </Box>
      {children}
    </Col>
  );
}
