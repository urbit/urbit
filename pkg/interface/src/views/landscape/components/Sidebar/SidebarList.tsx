import React, { useMemo } from "react";
import { alphabeticalOrder } from "~/logic/lib/util";
import { Associations, AppAssociations, Workspace, Groups } from "~/types";
import { SidebarAppConfigs, SidebarListConfig, SidebarSort } from "./types";
import { SidebarItem } from "./SidebarItem";

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
    const aModule = aAssoc?.metadata?.module || aAssoc?.["app-name"];
    const bModule = bAssoc?.metadata?.module || bAssoc?.["app-name"];

    const aUpdated = apps[aModule].lastUpdated(a);
    const bUpdated = apps[bModule].lastUpdated(b);

    return bUpdated - aUpdated || alphabetical(a, b);
  };

  return {
    asc: alphabetical,
    desc: (a, b) => alphabetical(b, a),
    lastUpdated,
  };
}

export function SidebarList(props: {
  apps: SidebarAppConfigs;
  config: SidebarListConfig;
  associations: Associations;
  groups: Groups;
  baseUrl: string;
  group?: string;
  selected?: string;
}) {
  const { selected, group, config } = props;
  const associations = {
      ...props.associations.chat,
      ...props.associations.publish,
      ...props.associations.link,
      ...props.associations.graph,
  };

  const ordered = Object.keys(associations)
    .filter((a) => {
      const assoc = associations[a];
      return group
        ? assoc["group-path"] === group
        : !(assoc["group-path"] in props.associations.contacts);
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
          />
        );
      })}
    </>
  );
}
