import BaseApi from './base';
import { StoreState } from '../store/type';
import { Patp, Path, PatpNoSig } from '~/types/noun';
import _ from 'lodash';
import {makeResource, resourceFromPath} from '../lib/group';
import {GroupPolicy, Enc, Post, NodeMap, Content} from '~/types';
import { numToUd, unixToDa } from '~/logic/lib/util';

export const createPost = (contents: Content[], parentIndex: string = '') => {
  return {
    author: `~${window.ship}`,
    index: parentIndex + '/' + unixToDa(Date.now()).toString(),
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
  return undefined;
}

export default class GraphApi extends BaseApi<StoreState> {

  private storeAction(action: any): Promise<any> {
    return this.action('graph-store', 'graph-update', action)
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
      "create": {
        resource,
        title,
        description,
        associated,
        "module": mod,
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
      "create": {
        resource,
        title,
        description,
        associated: { policy },
        "module": mod,
        mark: moduleToMark(mod)
      }
    });
  }

  joinGraph(ship: Patp, name: string) {
    const resource = makeResource(ship, name);
    return this.viewAction('graph-join', {
      join: {
        resource,
        ship,
      }
    });
  }

  deleteGraph(name: string) {
    const resource = makeResource(`~${window.ship}`, name);
    return this.viewAction('graph-delete', {
      "delete": {
        resource
      }
    });
  }

  leaveGraph(ship: Patp, name: string) {
    const resource = makeResource(ship, name);
    return this.viewAction('graph-leave', {
      "leave": {
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
    let nodes = {};
    const resource = { ship, name };
    nodes[post.index] = {
      post,
      children: { empty: null }
    };

    return this.hookAction(ship, {
      'add-nodes': {
        resource,
        nodes
      }
    });
  }

  addNodes(ship: Patp, name: string, nodes: Object) {
    return this.hookAction(ship, {
      'add-nodes': {
        resource: { ship, name },
        nodes
      }
    });
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

