import React from "react";

import { ChatResource } from "~/views/apps/chat/ChatResource";
import { LinkResource } from "~/views/apps/links/LinkResource";
import { PublishResource } from "~/views/apps/publish/PublishResource";

import { Association } from "~/types/metadata-update";
import { StoreState } from "~/logic/store/type";
import GlobalApi from "~/logic/api/global";
import { RouteComponentProps } from "react-router-dom";

type ResourceProps = StoreState & {
  association: Association;
  api: GlobalApi;
  baseUrl: string;
} & RouteComponentProps;

export function Resource(props: ResourceProps) {
  const { association } = props;
  const app = association["app-name"];
  if (app === "chat") {
    return <ChatResource {...props} />;
  }

  if (app === "publish") {
    return <PublishResource {...props} />;
  }
  if (app === 'link') {
    return <LinkResource {...props} />; 
  }
  return null;
}
