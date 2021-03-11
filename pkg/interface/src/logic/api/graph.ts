import BaseApi from './base';
import { StoreState } from '../store/type';
import { Patp, Path } from '@urbit/api';
import _ from 'lodash';
import { makeResource, resourceFromPath } from '../lib/group';
import { GroupPolicy, Enc, Post, Content } from '@urbit/api';
import { numToUd, unixToDa, decToUd, deSig, resourceAsPath } from '~/logic/lib/util';

export const createBlankNodeWithChildPost = (
  parentIndex = '',
  childIndex = '',
  contents: Content[]
) => {
  const date = unixToDa(Date.now()).toString();
  const nodeIndex = parentIndex + '/' + date;

  const childGraph = {};
  childGraph[childIndex] = {
    post: {
      author: `~${window.ship}`,
      index: nodeIndex + '/' + childIndex,
      'time-sent': Date.now(),
      contents,
      hash: null,
      signatures: []
    },
    children: null
  };

  return {
    post: {
      author: `~${window.ship}`,
      index: nodeIndex,
      'time-sent': Date.now(),
      contents: [],
      hash: null,
      signatures: []
    },
    children: childGraph
  };
};

function markPending(nodes: any) {
  _.forEach(nodes, (node) => {
    node.post.author = deSig(node.post.author);
    node.post.pending = true;
    markPending(node.children || {});
  });
}

export const createPost = (
  contents: Content[],
  parentIndex = '',
  childIndex = 'DATE_PLACEHOLDER'
) => {
  if (childIndex === 'DATE_PLACEHOLDER') {
    childIndex = unixToDa(Date.now()).toString();
  }
  return {
    author: `~${window.ship}`,
    index: parentIndex + '/' + childIndex,
    'time-sent': Date.now(),
    contents,
    hash: null,
    signatures: []
  };
};

function moduleToMark(mod: string): string | undefined {
  if(mod === 'link') {
    return 'graph-validator-link';
  }
  if(mod === 'publish') {
    return 'graph-validator-publish';
  }
  if(mod === 'chat') {
    return 'graph-validator-chat';
  }
  return undefined;
}

export default class GraphApi extends BaseApi<StoreState> {
  joiningGraphs = new Set<string>();

  private storeAction(action: any): Promise<any> {
    return this.action('graph-store', 'graph-update', action);
  }

  private viewAction(threadName: string, action: any) {
    return this.spider('graph-view-action', 'json', threadName, action);
  }

  private hookAction(ship: Patp, action: any): Promise<any> {
    return this.action('graph-push-hook', 'graph-update', action);
  }

  createManagedGraph(
    name: string,
    title: string,
    description: string,
    group: Path,
    mod: string
  ) {
    const associated = { group: resourceFromPath(group) };
    const resource = makeResource(`~${window.ship}`, name);

    return this.viewAction('graph-create', {
      'create': {
        resource,
        title,
        description,
        associated,
        'module': mod,
        mark: moduleToMark(mod)
      }
    });
  }

  createUnmanagedGraph(
    name: string,
    title: string,
    description: string,
    policy: Enc<GroupPolicy>,
    mod: string
  ) {
    const resource = makeResource(`~${window.ship}`, name);

    return this.viewAction('graph-create', {
      'create': {
        resource,
        title,
        description,
        associated: { policy },
        'module': mod,
        mark: moduleToMark(mod)
      }
    });
  }

  joinGraph(ship: Patp, name: string) {
    const resource = makeResource(ship, name);
    const rid = resourceAsPath(resource);
    if(this.joiningGraphs.has(rid)) {
      return Promise.resolve();
    }
    this.joiningGraphs.add(rid);
    return this.viewAction('graph-join', {
      join: {
        resource,
        ship
      }
    }).then((res) => {
      this.joiningGraphs.delete(rid);
      return res;
    });
  }

  deleteGraph(name: string) {
    const resource = makeResource(`~${window.ship}`, name);
    return this.viewAction('graph-delete', {
      'delete': {
        resource
      }
    });
  }

  leaveGraph(ship: Patp, name: string) {
    const resource = makeResource(ship, name);
    return this.viewAction('graph-leave', {
      'leave': {
        resource
      }
    });
  }

  groupifyGraph(ship: Patp, name: string, toPath?: string) {
    const resource = makeResource(ship, name);
    const to = toPath && resourceFromPath(toPath);

    return this.viewAction('graph-groupify', {
      groupify: {
        resource,
        to
      }
    });
  }

  eval(cord: string) {
    return this.spider('graph-view-action', 'tang', 'graph-eval', {
      eval: cord
    });
  }


  addGraph(ship: Patp, name: string, graph: any, mark: any) {
    return this.storeAction({
      'add-graph': {
        resource: { ship, name },
        graph,
        mark
      }
    });
  }

  addPost(ship: Patp, name: string, post: Post) {
    const nodes = {};
    nodes[post.index] = {
      post,
      children: null
    };
    return this.addNodes(ship, name, nodes);
  }

  addNode(ship: Patp, name: string, node: Object) {
    const nodes = {};
    nodes[node.post.index] = node;

    return this.addNodes(ship, name, nodes);
  }

  addNodes(ship: Patp, name: string, nodes: Object) {
    const action = {
      'add-nodes': {
        resource: { ship, name },
        nodes
      }
    };

    const pendingPromise = this.spider(
      'graph-update',
      'graph-view-action',
      'graph-add-nodes',
      action
    );

    markPending(action['add-nodes'].nodes);
    action['add-nodes'].resource.ship =
      action['add-nodes'].resource.ship.slice(1);

    this.store.handleEvent({ data: {
      'graph-update': action
    } });

    return pendingPromise;
    /* TODO: stop lying to our users about pending states
    return pendingPromise.then((pendingHashes) => {
      for (let index in action['add-nodes'].nodes) {
        action['add-nodes'].nodes[index].post.hash =
          pendingHashes['pending-indices'][index] || null;
      }

      this.store.handleEvent({ data: {
        'graph-update': {
          'pending-indices': pendingHashes['pending-indices'],
          ...action
        }
      } });
    });
    */
  }

  removeNodes(ship: Patp, name: string, indices: string[]) {
    return this.hookAction(ship, {
      'remove-nodes': {
        resource: { ship, name },
        indices
      }
    });
  }

  getKeys() {
    return this.scry<any>('graph-store', '/keys')
      .then((keys) => {
        this.store.handleEvent({
          data: keys
        });
      });
  }

  getTags() {
    return this.scry<any>('graph-store', '/tags')
      .then((tags) => {
        this.store.handleEvent({
          data: tags
        });
      });
  }

  getTagQueries() {
    return this.scry<any>('graph-store', '/tag-queries')
      .then((tagQueries) => {
        this.store.handleEvent({
          data: tagQueries
        });
      });
  }

  getGraph(ship: string, resource: string) {
    return this.scry<any>('graph-store', `/graph/${ship}/${resource}`)
      .then((graph) => {
        this.store.handleEvent({
          data: graph
        });
      });
  }

  async getNewest(ship: string, resource: string, count: number, index = '') {
    const data = await this.scry<any>('graph-store', `/newest/${ship}/${resource}/${count}${index}`);
    this.store.handleEvent({ data });
  }

  async getOlderSiblings(ship: string, resource: string, count: number, index = '') {
    const idx = index.split('/').map(decToUd).join('/');
    const data = await this.scry<any>('graph-store',
       `/node-siblings/older/${ship}/${resource}/${count}${idx}`
     );
    this.store.handleEvent({ data });
  }

  async getYoungerSiblings(ship: string, resource: string, count: number, index = '') {
    const idx = index.split('/').map(decToUd).join('/');
    const data = await this.scry<any>('graph-store',
       `/node-siblings/younger/${ship}/${resource}/${count}${idx}`
     );
    this.store.handleEvent({ data });
  }

  getGraphSubset(ship: string, resource: string, start: string, end: string) {
    return this.scry<any>(
      'graph-store',
      `/graph-subset/${ship}/${resource}/${end}/${start}`
    ).then((subset) => {
      this.store.handleEvent({
        data: subset
      });
    });
  }

  getNode(ship: string, resource: string, index: string) {
    const idx = index.split('/').map(numToUd).join('/');
    return this.scry<any>(
      'graph-store',
      `/node/${ship}/${resource}${idx}`
    ).then((node) => {
      this.store.handleEvent({
        data: node
      });
    });
  }
}

