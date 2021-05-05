import React, { ReactElement, useCallback, useEffect } from 'react';
import { Associations, AppAssociations, Groups, Rolodex } from '@urbit/api';

import { alphabeticalOrder, getResourcePath, modulo } from '~/logic/lib/util';
import { SidebarAppConfigs, SidebarListConfig, SidebarSort } from './types';
import { SidebarItem } from './SidebarItem';
import { Workspace } from '~/types/workspace';
import useMetadataState from '~/logic/state/metadata';
import useGraphState from '~/logic/state/graph';
import {useHistory} from 'react-router';
import {useShortcut} from '~/logic/lib/shortcutContext';

function sidebarSort(
  associations: AppAssociations,
  apps: SidebarAppConfigs
): Record<SidebarSort, (a: string, b: string) => number> {
  const alphabetical = (a: string, b: string) => {
    const aAssoc = associations[a];
    const bAssoc = associations[b];
    const aTitle = aAssoc?.metadata?.title || b;
    const bTitle = bAssoc?.metadata?.title || b;

    return alphabeticalOrder(aTitle, bTitle);
  };

  const lastUpdated = (a: string, b: string) => {
    const aAssoc = associations[a];
    const bAssoc = associations[b];
    const aAppName = aAssoc?.['app-name'];
    const bAppName = bAssoc?.['app-name'];

    const aUpdated = apps[aAppName]?.lastUpdated(a) || 0;
    const bUpdated = apps[bAppName]?.lastUpdated(b) || 0;

    return bUpdated - aUpdated || alphabetical(a, b);
  };

  return {
    asc: alphabetical,
    lastUpdated
  };
}

export function SidebarList(props: {
  apps: SidebarAppConfigs;
  config: SidebarListConfig;
  baseUrl: string;
  group?: string;
  selected?: string;
  workspace: Workspace;
}): ReactElement {
  const { selected, group, config, workspace } = props;
  const associationState = useMetadataState(state => state.associations);
  const graphKeys = useGraphState(s => s.graphKeys);
  const associations = { ...associationState.graph };

  const ordered = Object.keys(associations)
    .filter((a) => {
      const assoc = associations[a];
      if (workspace?.type === 'messages') {
        return (
          !(assoc.group in associationState.groups) &&
          'graph' in assoc.metadata.config &&
          assoc.metadata.config.graph === 'chat'
        );
      } else {
        return group ? (
          assoc.group === group &&
          !assoc.metadata.hidden
        ) : (
          !(assoc.group in associationState.groups) &&
          'graph' in assoc.metadata.config &&
          assoc.metadata.config.graph !== 'chat' &&
          !assoc.metadata.hidden
        );
      }
    })
    .sort(sidebarSort(associations, props.apps)[config.sortBy]);

  const history = useHistory();

  const cycleChannels = useCallback((backward: boolean) => {
    const idx = ordered.findIndex(s => s === selected);
    const offset = backward ? -1 : 1 

    const newIdx = modulo(idx+offset, ordered.length - 1);
    const { metadata, resource } = associations[ordered[newIdx]];
    const joined = graphKeys.has(resource.slice(7));
    const path = getResourcePath(workspace, resource, joined, metadata.config.graph)
    history.push(path)
  }, [selected, history.push]);

  useShortcut('ctrl+n', useCallback((e: KeyboardEvent) => {
    cycleChannels(false);
    e.preventDefault();
  }, [cycleChannels]));

  useShortcut('ctrl+p', useCallback((e: KeyboardEvent) => {
    cycleChannels(true);
    e.preventDefault();
  }, [cycleChannels]))


  return (
    <>
      {ordered.map((path) => {
        const assoc = associations[path];
        return (
          <SidebarItem
            key={path}
            path={path}
            selected={path === selected}
            association={assoc}
            apps={props.apps}
            hideUnjoined={config.hideUnjoined}
            workspace={workspace}
          />
        );
      })}
    </>
  );
}
