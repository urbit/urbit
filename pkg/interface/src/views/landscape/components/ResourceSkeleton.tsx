import React, { useCallback, ReactNode } from "react";
import { Row, Box, Col, Text } from "@tlon/indigo-react";
import styled from "styled-components";
import { Link } from "react-router-dom";

import { ChatResource } from "~/views/apps/chat/ChatResource";
import { PublishResource } from "~/views/apps/publish/PublishResource";

import { Association } from "~/types/metadata-update";
import { StoreState } from "~/logic/store/type";
import GlobalApi from "~/logic/api/global";
import { RouteComponentProps, Route, Switch } from "react-router-dom";
import { ChannelSettings } from "./ChannelSettings";
import { ChannelMenu } from "./ChannelMenu";

const TruncatedBox = styled(Box)`
  white-space: nowrap;
  text-overflow: ellipsis;
  overflow: hidden;
`;

type ResourceSkeletonProps = {
  association: Association;
  api: GlobalApi;
  baseUrl: string;
  children: ReactNode;
  atRoot?: boolean;
  title?: string;
};

export function ResourceSkeleton(props: ResourceSkeletonProps) {
  const { association, api, children, atRoot } = props;
  const app = association?.metadata?.module || association["app-name"];
  const appPath = association["app-path"];
  const selectedGroup = association["group-path"];
  const title = props.title || association?.metadata?.title;
  return (
    <Col width="100%" height="100%" overflowY="hidden">
      <Box
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
            <Link to={`/~landscape${selectedGroup}`}> {"<- Back"}</Link>
          </Box>
        ) : (
          <Box
            color="blue"
            pr={2}
            mr={2}
          >
            <Link to={`/~landscape${selectedGroup}/resource/${app}${appPath}`}>
              <Text color="blue">Go back to channel</Text>
            </Link>
          </Box>
        )}
        
        {atRoot && (
          <>
            <Box pr={1} mr={2}>
              <Text>{title}</Text>
            </Box>
            <TruncatedBox
              display={["none", "block"]}
              maxWidth="60%"
              flexShrink={1}
              title={association?.metadata?.description}
              color="gray"
            >
              {association?.metadata?.description}
            </TruncatedBox>
            <Box flexGrow={1} />
            <ChannelMenu association={association} api={api} />
          </>
        )}
      </Box>
      {children}
    </Col>
  );
}
