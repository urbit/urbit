import BaseApi from './base';
import { StoreState } from '../store/type';
import { Patp, Path, PatpNoSig } from '~/types/noun';


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

  addPost(ship: Patp, name: string, post: Object) {
    let nodes = {};
    nodes[post.index] = {
      post,
      children: { empty: null }
    };

    this.storeAction({
      'add-nodes': {
        resource: { ship, name },
        nodes
      }
    });
  }

  addNodes(ship: Patp, name: string, nodes: Object) {
    this.storeAction({
      'add-nodes': {
        resource: { ship, name },
        nodes
      }
    });
  }

  removeNodes(ship: Patp, name: string, indices: string[]) {
    this.storeAction({
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

  getGraphSubset(ship: string, resource: string, start: string, end: start) {
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

