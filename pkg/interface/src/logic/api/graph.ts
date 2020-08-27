import BaseApi from './base';
import { StoreState } from '../store/type';
import { Patp, Path, PatpNoSig } from '~/types/noun';


export const createPost = (contents: Object[], parentIndex: string) => {
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
        console.log(keys);
      });
  }

  getTags() {
    this.scry<any>('graph-store', '/tags')
      .then((tags) => {
        console.log(tags);
      });
  }

  getTagQueries() {
    this.scry<any>('graph-store', '/tag-queries')
      .then((tagQueries) => {
        console.log(tagQueries);
      });
  }

  getGraph(ship: string, resource: string) {
    this.scry<any>('graph-store', `/graph/${ship}/${resource}`)
      .then((graph) => {
        console.log(graph);
      });
  }

  getGraphSubset(ship: string, resource: string, start: string, end: start) {
    this.scry<any>(
      'graph-store',
      `/graph-subset/${ship}/${resource}/${start}/${end}`
    ).then((subset) => {
      console.log(subset);
    });
  }

  getNode(ship: string, resource: string, index: string) {
    this.scry<any>(
      'graph-store',
      `/node/${ship}/${resource}/${index}`
    ).then((node) => {
      console.log(node);
    });
  }

  getPost(ship: string, resource: string, index: string) {
    this.scry<any>(
      'graph-store',
      `/post/${ship}/${resource}/${index}`
    ).then((post) => {
      console.log(post);
    });
  }

  getNodeChildren(ship: string, resource: string, index: string) {
    this.scry<any>(
      'graph-store',
      `/node-children/${ship}/${resource}/${index}`
    ).then((children) => {
      console.log(children);
    });
  }

  getNodeChildrenSubset(ship: string, resource: string,
                        start: string, end: string, index: string) {
    this.scry<any>(
      'graph-store',
      `/node-children-subset/${ship}/${resource}/${start}/${end}/${index}`
    ).then((subset) => {
      console.log(subset);
    });
  }

}

