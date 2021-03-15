import React, { ReactElement, ReactNode, useMemo } from 'react';

import { Groups, Graphs, Invites, Rolodex, Path, AppName } from '@urbit/api';
import { Associations } from '@urbit/api/metadata';

import { Sidebar } from './Sidebar/Sidebar';
import GlobalApi from '~/logic/api/global';
import { useGraphModule } from './Sidebar/Apps';
import { Body } from '~/views/components/Body';
import { Workspace } from '~/types/workspace';
import useGraphState from '~/logic/state/graph';
import useHarkState from '~/logic/state/hark';
import ErrorBoundary from '~/views/components/ErrorBoundary';

interface SkeletonProps {
  children: ReactNode;
  recentGroups: string[];
  selected?: string;
  selectedApp?: AppName;
  baseUrl: string;
  mobileHide?: boolean;
  api: GlobalApi;
  workspace: Workspace;
}

export function Skeleton(props: SkeletonProps): ReactElement {
  const graphs = useGraphState(state => state.graphs);
  const graphKeys = useGraphState(state => state.graphKeys);
  const unreads = useHarkState(state => state.unreads);
  const graphConfig = useGraphModule(graphKeys, graphs, unreads.graph);
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
      <ErrorBoundary>
        <Sidebar
          api={props.api}
          recentGroups={props.recentGroups}
          selected={props.selected}
          apps={config}
          baseUrl={props.baseUrl}
          mobileHide={props.mobileHide}
          workspace={props.workspace}
          history={props.history}
          />
      </ErrorBoundary>
      {props.children}
    </Body>
  );
}
