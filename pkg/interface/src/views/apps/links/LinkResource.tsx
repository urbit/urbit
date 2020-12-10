import React, { useEffect } from "react";
import { Box, Row, Col, Center, LoadingSpinner, Text } from "@tlon/indigo-react";
import { Switch, Route, Link } from "react-router-dom";
import bigInt from 'big-integer';

import GlobalApi from "~/logic/api/global";
import { StoreState } from "~/logic/store/type";
import { uxToHex } from '~/logic/lib/util';
import { RouteComponentProps } from "react-router-dom";

import { LinkItem } from "./components/LinkItem";
import { LinkSubmit } from "./components/link-submit";
import { LinkPreview } from "./components/link-preview";
import { Comments } from "~/views/components/comments";

import "./css/custom.css";

type LinkResourceProps = StoreState & {
  association: Association;
  api: GlobalApi;
  baseUrl: string;
} & RouteComponentProps;

export function LinkResource(props: LinkResourceProps) {
  const {
    association,
    api,
    baseUrl,
    graphs,
    contacts,
    groups,
    associations,
    graphKeys,
    s3,
    hideAvatars,
    hideNicknames,
    remoteContentPolicy,
    history
  } = props;

  const appPath = association["app-path"];

  const relativePath = (p: string) => `${baseUrl}/resource/link${appPath}${p}`;

  const [, , ship, name] = appPath.split("/");
  const resourcePath = `${ship.slice(1)}/${name}`;
  const resource = associations.graph[appPath]
    ? associations.graph[appPath]
    : { metadata: {} };
  const contactDetails = contacts[resource["group-path"]] || {};
  const group = groups[resource["group-path"]] || {};
  const graph = graphs[resourcePath] || null;

  useEffect(() => {
    api.graph.getGraph(ship, name);
  }, [association]);

  const resourceUrl = `${baseUrl}/resource/link${appPath}`;
  if (!graph) {
    return <Center width='100%' height='100%'><LoadingSpinner/></Center>;
  }

  return (
    <Col alignItems="center" height="100%" width="100%" overflowY="auto">
      <Switch>
        <Route
          exact
          path={relativePath("")}
          render={(props) => {
            return (
              <Col width="100%" p={4} alignItems="center" maxWidth="768px">
                <Col width="100%" flexShrink='0'>
                  <LinkSubmit s3={s3} name={name} ship={ship.slice(1)} api={api} />
                </Col>
                {Array.from(graph).map(([date, node]) => {
                  const contact = contactDetails[node.post.author];
                  return (
                    <LinkItem
                      contacts={contacts}
                      key={date.toString()}
                      resource={resourcePath}
                      node={node}
                      hideAvatars={hideAvatars}
                      hideNicknames={hideNicknames}
                      remoteContentPolicy={remoteContentPolicy}
                      baseUrl={resourceUrl}
                      group={group}
                      path={resource["group-path"]}
                      api={api}
                      mb={3}
                    />
                  );
                })}
              </Col>
            );
          }}
        />
        <Route
          path={relativePath("/:index(\\d+)/:commentId?")}
          render={(props) => {
            const index = bigInt(props.match.params.index);
            const editCommentId = props.match.params.commentId || null;

            if (!index) {
              return <div>Malformed URL</div>;
            }

            const node = !!graph ? graph.get(index) : null;

            if (!node) {
              return <Box>Not found</Box>;
            }

            const contact = contactDetails[node.post.author];

            return (
              <Col width="100%" p={3} maxWidth="768px">
                <Link to={resourceUrl}><Text bold>{"<- Back"}</Text></Link>
                <LinkItem
                  contacts={contacts}
                  key={node.post.index}
                  resource={resourcePath}
                  node={node}
                  hideAvatars={hideAvatars}
                  hideNicknames={hideNicknames}
                  remoteContentPolicy={remoteContentPolicy}
                  baseUrl={resourceUrl}
                  group={group}
                  path={resource["group-path"]}
                  api={api}
                  mt={3}
                />
                <Comments
                  ship={ship}
                  name={name}
                  comments={node}
                  resource={resourcePath}
                  contacts={contactDetails}
                  api={api}
                  hideAvatars={hideAvatars}
                  hideNicknames={hideNicknames}
                  remoteContentPolicy={remoteContentPolicy}
                  editCommentId={editCommentId}
                  history={props.history}
                  baseUrl={`${resourceUrl}/${props.match.params.index}`}
                  association={association}
                  group={group}
                />
              </Col>
            );
          }}
        />
      </Switch>
    </Col>
  );
}
