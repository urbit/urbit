import React, { ReactElement, ReactNode, useMemo } from 'react';

import { Groups, Graphs, Invites, Rolodex, Path, AppName } from '@urbit/api';
import { Associations } from '@urbit/api/metadata';

import { Sidebar } from './Sidebar/Sidebar';
import GlobalApi from '~/logic/api/global';
import GlobalSubscription from '~/logic/subscription/global';
import { useGraphModule } from './Sidebar/Apps';
import { Body } from '~/views/components/Body';
import { Workspace } from '~/types/workspace';

interface SkeletonProps {
  contacts: Rolodex;
  children: ReactNode;
  recentGroups: string[];
  groups: Groups;
  associations: Associations;
  graphKeys: Set<string>;
  graphs: Graphs;
  linkListening: Set<Path>;
  invites: Invites;
  selected?: string;
  selectedApp?: AppName;
  baseUrl: string;
  mobileHide?: boolean;
  api: GlobalApi;
  subscription: GlobalSubscription;
  includeUnmanaged: boolean;
  workspace: Workspace;
  unreads: unknown;
}

export function Skeleton(props: SkeletonProps): ReactElement {
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
