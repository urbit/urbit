import React, { ReactElement } from 'react';
import { Associations, AppAssociations, Groups, Rolodex } from '@urbit/api';

import { alphabeticalOrder } from '~/logic/lib/util';
import { SidebarAppConfigs, SidebarListConfig, SidebarSort } from './types';
import { SidebarItem } from './SidebarItem';
import { Workspace } from '~/types/workspace';

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
  contacts: Rolodex;
  config: SidebarListConfig;
  associations: Associations;
  groups: Groups;
  baseUrl: string;
  group?: string;
  selected?: string;
  workspace: Workspace;
}): ReactElement {
  const { selected, group, config, workspace } = props;
  const associations = { ...props.associations.graph };

  const ordered = Object.keys(associations)
    .filter((a) => {
      const assoc = associations[a];
      if (workspace?.type === 'messages') {
        return (
          !(assoc.group in props.associations.groups) &&
          'graph' in assoc.metadata.config &&
          assoc.metadata.config.graph === 'chat'
        );
      } else {
        return group ? assoc.group === group : (
          !(assoc.group in props.associations.groups) &&
          'graph' in assoc.metadata.config &&
          assoc.metadata.config.graph !== 'chat'
        );
      }
    })
    .sort(sidebarSort(associations, props.apps)[config.sortBy]);

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
            groups={props.groups}
            contacts={props.contacts}
            workspace={workspace}
          />
        );
      })}
    </>
  );
}
