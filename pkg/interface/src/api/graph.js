import BaseApi from './base';

class PrivateHelper extends BaseApi {
  graphAction(data) {
    this.action('graph-store', 'graph-action', data);
  }

  addGraph(resource, graph) {
    this.graphAction({
      'add-graph': {
        resource,
        graph
      }
    });
  }

  removeGraph(resource) {
    this.graphAction({
      'remove-graph': {
        resource
      }
    });
  }

  addNodes(resource, nodes) {
    this.graphAction({
      'add-nodes': {
        resource,
        nodes
      }
    });
  }

  removeNodes(resource, indices) {
    this.graphAction({
      'remove-nodes': {
        resource,
        indices
      }
    });
  }

  addSignatures() {
    this.graphAction();
  }

  removeSignatures() {
    this.graphAction();
  }

  addTag() {
    this.graphAction();
  }

  removeTag() {
    this.graphAction();
  }
}

export default class GraphApi {
  constructor(ship, channel, store) {
    const helper = new PrivateHelper(ship, channel, store);

    this.ship = ship;
    this.subscribe = helper.subscribe.bind(helper);

    this.addGraph = helper.addGraph.bind(helper);
    this.removeGraph = helper.removeGraph.bind(helper);

    this.addNodes = helper.addNodes.bind(helper);
    this.removeNodes = helper.removeNodes.bind(helper);

    this.addSignatures = helper.addSignatures.bind(helper);
    this.removeSignatures = helper.removeSignatures.bind(helper);
   
    this.addTag = helper.addTag.bind(helper);
    this.removeTag = helper.removeTag.bind(helper);
  }
}

