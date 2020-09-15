import BaseApi from './base';
import { StoreState } from '../store/type';
import { Patp, Path, PatpNoSig } from '~/types/noun';
import _ from 'lodash';
import {makeResource, resourceFromPath} from '../lib/group';
import {GroupPolicy, Enc, Post, NodeMap} from '~/types';

export const createPost = (contents: Object[], parentIndex: string = '') => {
  return {
    author: `~${window.ship}`,
    index: parentIndex + '/' + Date.now(),
    'time-sent': Date.now(),
    contents,
    hash: null,
    signatures: []
  };
};

export default class GraphApi extends BaseApi<StoreState> {

  private storeAction(action: any): Promise<any> {
    return this.action('graph-store', 'graph-update', action)
  }

  private viewAction(threadName: string, action: any) {
    return this.spider('graph-view-action', 'json', threadName, action);
  }

  createManagedGraph(name: string, title: string, description: string, app: string, group: Path) {
    const associated = { group: resourceFromPath(group) };

    const resource = makeResource(`~${window.ship}`, name);
    return this.viewAction('graph-create', {
      "create": {
        resource,
        title,
        description,
        app,
        associated
      }
    });
  }

  createUnmanagedGraph(name: string, title: string, description: string, app: string, policy: Enc<GroupPolicy>) {

    const resource = makeResource(`~${window.ship}`, name);
    return this.viewAction('graph-create', {
      "create": {
        resource,
        title,
        description,
        app,
        associated: { policy }
      }
    });
  }

  joinGraph(ship: Patp, name: string, app: string) {
    const resource = makeResource(ship, name);
    return this.viewAction('graph-join', {
      join: {
        resource,
        ship,
        app
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

  groupifyGraph(ship: Patp, name: string, app: string, toPath?: string) {
    const resource = makeResource(ship, name);
    const to = toPath && resourceFromPath(toPath);

    return this.viewAction('graph-groupify', {
      groupify: {
        resource,
        app,
        to
      }
    });
  }

  addGraph(ship: Patp, name: string, graph: any, mark: any) {
    this.storeAction({
      'add-graph': {
        resource: { ship, name },
        graph,
        mark
      }
    });
  }

  removeGraph(ship: Patp, name: string) {
    this.storeAction({
      'remove-graph': {
        resource: { ship, name }
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

    return this.storeAction({
      'add-nodes': {
        resource,
        nodes
      }
    });
  }

  addNodes(ship: Patp, name: string, nodes: NodeMap) {
    return this.storeAction({
      'add-nodes': {
        resource: { ship, name },
        nodes
      }
    });
  }

  removeNodes(ship: Patp, name: string, indices: string[]) {
    return this.storeAction({
      'remove-nodes': {
        resource: { ship, name },
        indices
      }
    });
  }

  getKeys() {
    this.scry<any>('graph-store', '/keys')
      .then((keys) => {
        this.store.handleEvent({
          data: keys
        });
      });
  }

  getTags() {
    this.scry<any>('graph-store', '/tags')
      .then((tags) => {
        this.store.handleEvent({
          data: tags
        });
      });
  }

  getTagQueries() {
    this.scry<any>('graph-store', '/tag-queries')
      .then((tagQueries) => {
        this.store.handleEvent({
          data: tagQueries
        });
      });
  }

  getGraph(ship: string, resource: string) {
    this.scry<any>('graph-store', `/graph/${ship}/${resource}`)
      .then((graph) => {
        this.store.handleEvent({
          data: graph
        });
      });
  }

  getGraphSubset(ship: string, resource: string, start: string, end: string) {
    this.scry<any>(
      'graph-store',
      `/graph-subset/${ship}/${resource}/${end}/${start}`
    ).then((subset) => {
      this.store.handleEvent({
        data: subset
      });
    });
  }

  getNode(ship: string, resource: string, index: string) {
    return this.scry<any>(
      'graph-store',
      `/node/${ship}/${resource}${index}`
    ).then((node) => {
      this.store.handleEvent({
        data: node
      });
    });
  }
}

