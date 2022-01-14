import React, { ReactElement, ReactNode, useCallback, useState } from 'react';
import { Sidebar } from './Sidebar/Sidebar';
import { Workspace } from '~/types/workspace';
import { Body } from '~/views/components/Body';
import ErrorBoundary from '~/views/components/ErrorBoundary';
import { useShortcut } from '~/logic/state/settings';
import { isMobile } from '~/views/apps/chat/components/ChatEditor';

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

  const hideMobileSidebar = props.mobileHide && isMobile;

  return (
    <Body
      display="grid"
      gridTemplateColumns={
        sidebar
        ?  ['100%', 'minmax(150px, 1fr) 3fr', 'minmax(250px, 1fr) 4fr', 'minmax(300px, 1fr) 5fr']
        : '100%'
      }
      gridTemplateRows="100%"
    >
      <ErrorBoundary>
        { sidebar && !hideMobileSidebar && (
          <Sidebar
            recentGroups={props.recentGroups}
            selected={props.selected}
            baseUrl={props.baseUrl}
            workspace={props.workspace}
          />
        )}
      </ErrorBoundary>
      {props.children}
    </Body>
  );
});
