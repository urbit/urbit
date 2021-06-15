import React, { ReactElement, ReactNode, useCallback, useState } from 'react';
import { Sidebar } from './Sidebar/Sidebar';
import { Workspace } from '~/types/workspace';
import { Body } from '~/views/components/Body';
import ErrorBoundary from '~/views/components/ErrorBoundary';
import { useShortcut } from '~/logic/state/settings';

interface SkeletonProps {
  children: ReactNode;
  recentGroups: string[];
  selected?: string;
  baseUrl: string;
  mobileHide?: boolean;
  workspace: Workspace;
}

export const Skeleton = React.memo((props: SkeletonProps): ReactElement => {
  const [sidebar, setSidebar] = useState(true);
  useShortcut('hideSidebar', useCallback(() => {
    setSidebar(s => !s);
  }, []));

  return !sidebar ? (<Body> {props.children} </Body>) : (
    <Body
      display="grid"
      gridTemplateColumns={
        ['100%', 'minmax(150px, 1fr) 3fr', 'minmax(250px, 1fr) 4fr']
      }
      gridTemplateRows="100%"
    >
      <ErrorBoundary>
        <Sidebar
          recentGroups={props.recentGroups}
          selected={props.selected}
          baseUrl={props.baseUrl}
          mobileHide={props.mobileHide}
          workspace={props.workspace}
        />
      </ErrorBoundary>
      {props.children}
    </Body>
  );
});
