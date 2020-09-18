import React from "react";

import { ChatResource } from "~/views/apps/chat/ChatResource";
import { PublishResource } from "~/views/apps/publish/PublishResource";
import { LinkResource } from "~/views/apps/links/LinkResource";

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
  if (app === 'graph') {
    return <LinkResource {...props} />;
  }
  return null;
}
