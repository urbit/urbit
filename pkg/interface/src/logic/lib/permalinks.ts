
import {
  Association,
  resourceFromPath,
  Group,
  ReferenceContent,
} from "@urbit/api";

export function getPermalinkForGraph(
  group: string,
  graph: string,
  index = ""
) {
  const groupLink = getPermalinkForAssociatedGroup(group);
  const { ship, name } = resourceFromPath(graph);
  return `${groupLink}/graph/${ship}/${name}${index}`;
}

function getPermalinkForAssociatedGroup(group: string) {
  const { ship, name } = resourceFromPath(group);
  return `web+urbit-graph://group/${ship}/${name}`;
}


type Permalink = GraphPermalink | GroupPermalink;

interface GroupPermalink {
  type: "group";
  group: string;
  link: string;
}
interface GraphPermalink {
  type: "graph";
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
  if (kind !== "graph") {
    return null;
  }
  const graph = `/ship/${ship}/${name}`;
  return {
    type: "graph",
    link: link.slice(11),
    graph,
    group,
    index: `/${index.join("/")}`,
  };
}

export function permalinkToReference({ any }: any): any {
  //TODO write this function
  return { reference: '' }
}

export function referenceToPermalink({ reference }: ReferenceContent): Permalink {
  if('graph' in reference) {
    const { graph, group, index } = reference.graph;
    const link = `web+urbit-graph://group${group.slice(5)}/graph${graph.slice(5)}${index}`;
    return {
      type: 'graph',
      link,
      ...reference.graph
    };
  } else {
    const link = `web+urbit-graph://group${reference.group.slice(5)}`;
    return {
      type: 'group',
      link,
      ...reference
    }
  }
}

export function parsePermalink(url: string): Permalink | null {
  const [kind, ...rest] = url.slice(12).split("/");
  if (kind === "group") {
    const [ship, name, ...graph] = rest;
    const group = `/ship/${ship}/${name}`;
    if (graph.length > 0) {
      return parseGraphPermalink(url, group, graph);
    }
    return {
      type: "group",
      group,
      link: url.slice(11),
    };
  }
  return null;
}
