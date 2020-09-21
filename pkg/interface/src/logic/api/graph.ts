import BaseApi from './base';
import { StoreState } from '../store/type';
import { Patp, Path, PatpNoSig } from '~/types/noun';
import _ from 'lodash';
import {makeResource, resourceFromPath} from '../lib/group';
import {GroupPolicy, Enc, Post} from '~/types';
import { deSig } from '~/logic/lib/util';

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

  private hookAction(ship: Patp, action: any): Promise<any> {
    return this.action('graph-push-hook', 'graph-update', action, deSig(ship));
  }

  createManagedGraph(
    name: string,
    title: string,
    description: string,
    group: Path
  ) {
    const associated = { group: resourceFromPath(group) };
    const resource = makeResource(`~${window.ship}`, name);

    return this.viewAction('graph-create', {
      "create": {
        resource,
        title,
        description,
        associated
      }
    });
  }

  createUnmanagedGraph(
    name: string,
    title: string,
    description: string,
    policy: Enc<GroupPolicy>
  ) {
    const resource = makeResource(`~${window.ship}`, name);

    return this.viewAction('graph-create', {
      "create": {
        resource,
        title,
        description,
        associated: { policy }
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
    this.hookAction(ship, {
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
    this.scry<any>(
      'graph-store',
      `/node/${ship}/${resource}/${index}`
    ).then((node) => {
      this.store.handleEvent({
        data: node
      });
    });
  }
}

