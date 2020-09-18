import React, { useEffect } from "react";
import { Box, Col } from "@tlon/indigo-react";

import { LinkItem } from "./components/lib/link-item";
import { LinkSubmit } from "./components/lib/link-submit";
import { LinkList } from "./components/links-list";
import GlobalApi from "~/logic/api/global";
import { StoreState } from "~/logic/store/type";
import { Association } from "~/types";
import { RouteComponentProps } from "react-router-dom";

type LinkResourceProps = StoreState & {
  association: Association;
  api: GlobalApi;
  baseUrl: string;
} & RouteComponentProps;

export function LinkResource(props: LinkResourceProps) {
  const { association, api, baseUrl, associations, graphs, contacts } = props;
  const appPath = association["app-path"];
  const [,, ship, name] = appPath.split("/");
  const resource = appPath.slice(7);

  const contactDetails = contacts[association["group-path"]] || {};
  const graph = graphs[resource] || null;

  useEffect(() => {
    api.graph.getGraph(ship,name);

  }, [association, graphs]);

  if (!graph) {
    console.log(resource);
    return null;
  }

  return (
    <Box height="100%" width="100%" overflowY="auto">
      <Col p={2} mx="auto" maxWidth="450px">
        <LinkSubmit name={name} ship={ship} api={props.api} />
        {Array.from(graph.values()).map((node: any) => {
          console.log(node);
          const contact = contactDetails[node.post.author];
          return (
            <LinkItem
              resource={resource}
              node={node}
              nickname={contact?.nickname}
              hideAvatars={props.hideAvatars}
              hideNicknames={props.hideNicknames}
            />
          );
        })}
      </Col>
    </Box>
  );
}
