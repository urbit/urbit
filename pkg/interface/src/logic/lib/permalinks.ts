
import {
  ReferenceContent, resourceFromPath
} from '@urbit/api';
import _ from 'lodash';

export function getPermalinkForGraph(
  group: string,
  graph: string,
  index = ''
) {
  const groupLink = getPermalinkForAssociatedGroup(group);
  const { ship, name } = resourceFromPath(graph);
  return `${groupLink}/graph/${ship}/${name}${index}`;
}

function getPermalinkForAssociatedGroup(group: string) {
  const { ship, name } = resourceFromPath(group);
  return `web+urbitgraph://group/${ship}/${name}`;
}

type Permalink = AppPermalink | GraphPermalink | GroupPermalink;

export interface AppPermalink {
  type: 'app';
  link: string;
  ship: string;
  desk: string;
}

export interface GroupPermalink {
  type: 'group';
  group: string;
  link: string;
}

export interface GraphPermalink {
  type: 'graph';
  link: string;
  graph: string;
  group: string;
  index: string;
}

function parseGraphPermalink(
  link: string,
  group: string,
  segments: string[]
): GraphPermalink | null {
  const [kind, ship, name, ...index] = segments;
  if (kind !== 'graph') {
    return null;
  }
  const graph = `/ship/${ship}/${name}`;
  return {
    type: 'graph',
    link: link.slice(16),
    graph,
    group,
    index: `/${index.join('/')}`
  };
}

export function permalinkToReference(link: Permalink): ReferenceContent {
  switch (link.type) {
    case 'graph':
      return { reference: { graph: _.omit(link, 'type') } };
    case 'group':
      return { reference: { group: link.group } };
    case 'app':
      return { reference: { app: _.omit(link, 'type') } };
  }
}

export function referenceToPermalink({ reference }: ReferenceContent): Permalink {
  if('graph' in reference) {
    const { graph, group, index } = reference.graph;
    const link = `web+urbitgraph://group${group.slice(5)}/graph${graph.slice(5)}${index}`;
    return {
      type: 'graph',
      link,
      ...reference.graph
    };
  } else if ('app' in reference) {
    const { ship, desk } = reference.app;
    return {
      type: 'app',
      link: `web+urbitgraph://app/${ship}/${desk}`,
      ship,
      desk
    };
  } else {
    const link = `web+urbitgraph://group${reference.group.slice(5)}`;
    return {
      type: 'group',
      link,
      ...reference
    };
  }
}

export function parsePermalink(url: string): Permalink | null {
  const [kind, ...rest] = url.slice(17).split('/');

  if (kind === 'group') {
    const [ship, name, ...graph] = rest;
    const group = `/ship/${ship}/${name}`;
    if (graph.length > 0) {
      return parseGraphPermalink(url, group, graph);
    }
    return {
      type: 'group',
      group,
      link: url.slice(11)
    };
  }

  if (kind === 'app') {
    const [ship, desk] = rest;
    return {
      type: kind,
      link: url,
      ship,
      desk
    };
  }

  return null;
}
