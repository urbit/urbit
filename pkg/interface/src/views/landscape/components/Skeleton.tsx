import React, { ReactNode, useEffect, useMemo } from 'react';
import { Box, Text } from '@tlon/indigo-react';
import { Link } from 'react-router-dom';

import { Sidebar } from './Sidebar/Sidebar';
import { Associations } from '~/types/metadata-update';
import { Notebooks } from '~/types/publish-update';
import GlobalApi from '~/logic/api/global';
import { Path, AppName } from '~/types/noun';
import { LinkCollections } from '~/types/link-update';
import GlobalSubscription from '~/logic/subscription/global';
import { Workspace, Groups, Graphs, Invites, Rolodex } from '~/types';
import { useGraphModule } from './Sidebar/Apps';
import { Body } from '~/views/components/Body';

interface SkeletonProps {
  contacts: Rolodex;
  children: ReactNode;
  recentGroups: string[];
  groups: Groups;
  associations: Associations;
  graphKeys: Set<string>;
  graphs: Graphs;
  linkListening: Set<Path>;
  links: LinkCollections;
  notebooks: Notebooks;
  invites: Invites;
  selected?: string;
  selectedApp?: AppName;
  baseUrl: string;
  mobileHide?: boolean;
  api: GlobalApi;
  subscription: GlobalSubscription;
  includeUnmanaged: boolean;
  workspace: Workspace;
  unreads: any;
}

export function Skeleton(props: SkeletonProps) {
  const graphConfig = useGraphModule(props.graphKeys, props.graphs, props.unreads.graph);
  const config = useMemo(
    () => ({
      graph: graphConfig
    }),
    [graphConfig]
  );

  return (
    <Body
      display="grid"
      gridTemplateColumns={
        ['100%', 'minmax(150px, 1fr) 3fr', 'minmax(250px, 1fr) 4fr']
      }
      gridTemplateRows="100%"
    >
      <Sidebar
        contacts={props.contacts}
        api={props.api}
        recentGroups={props.recentGroups}
        selected={props.selected}
        associations={props.associations}
        invites={props.invites}
        apps={config}
        baseUrl={props.baseUrl}
        groups={props.groups}
        mobileHide={props.mobileHide}
        workspace={props.workspace}
        history={props.history}
      />
      {props.children}
    </Body>
  );
}
