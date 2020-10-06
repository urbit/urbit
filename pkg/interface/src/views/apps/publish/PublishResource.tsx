import React from "react";
import { Box } from '@tlon/indigo-react';

import GlobalApi from "~/logic/api/global";
import { StoreState } from "~/logic/store/type";
import { Association } from "~/types";
import { RouteComponentProps } from "react-router-dom";
import { NotebookRoutes } from "./components/NotebookRoutes";

type PublishResourceProps = StoreState & {
  association: Association;
  api: GlobalApi;
  baseUrl: string;
} & RouteComponentProps;

export function PublishResource(props: PublishResourceProps) {
  const { association, api, baseUrl, notebooks } = props;
  const appPath = association["app-path"];
  const [, ship, book] = appPath.split("/");
  const notebook = notebooks[ship]?.[book];
  const notebookContacts = props.contacts[association["group-path"]];

  return (
    <Box height="100%" width="100%" overflowY="auto">
      <NotebookRoutes
        api={api}
        ship={ship}
        book={book}
        contacts={props.contacts}
        groups={props.groups}
        notebook={notebook}
        associations={props.associations}
        notebookContacts={notebookContacts}
        rootUrl={baseUrl}
        baseUrl={`${baseUrl}/resource/publish/${ship}/${book}`}
        history={props.history}
        match={props.match}
        location={props.location}
        hideAvatars={props.hideAvatars}
        hideNicknames={props.hideNicknames}
        remoteContentPolicy={props.remoteContentPolicy}
      />
    </Box>
  );
}
