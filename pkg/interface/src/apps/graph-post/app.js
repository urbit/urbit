import React from 'react';
import { Route } from 'react-router-dom';

import GraphApi from '../../api/graph';
import GraphStore from '../../store/graph';
import GraphSubscription from '../../subscription/graph';


import './css/custom.css';

import { Skeleton } from './components/skeleton';
import { Sidebar } from './components/sidebar';
import { PostScreen } from './components/post';
import { NodeTreeScreen } from './components/node-tree';
import { NewScreen } from './components/new';


export default class GraphPostApp extends React.Component {
  constructor(props) {
    super(props);
    this.store = new GraphStore();
    this.state = this.store.state;
    this.resetControllers();
  }

  resetControllers() {
    this.api = null;
    this.subscription = null;
  }

  componentDidMount() {
    document.title = 'OS1 - Chat';
    // preload spinner asset
    new Image().src = '/~landscape/img/Spinner.png';

    this.store.setStateHandler(this.setState.bind(this));
    const channel = new this.props.channel();
    this.api = new GraphApi(this.props.ship, channel, this.store);

    this.subscription = new GraphSubscription(this.store, this.api, channel);
    this.subscription.start();
  }

  componentWillUnmount() {
    this.subscription.delete();
    this.store.clear();
    this.store.setStateHandler(() => {});
    this.resetControllers();
  }

  render() {
    const { state, props } = this;

    const renderChannelSidebar = (props, resource) => (
      <Sidebar
        keys={state.keys}
        api={this.api}
        resource={resource}
        {...props}
      />
    );

    return (
      <div>
        <Route
          exact
          path="/~post"
          render={(props) => {
            return (
              <Skeleton
                chatHideonMobile={true}
                sidebarShown={state.sidebarShown}
                sidebar={renderChannelSidebar(props)}
              >
                <div className="h-100 w-100 overflow-x-hidden flex flex-column bg-white bg-gray0-d">
                  <div className="pl3 pr3 pt2 dt pb3 w-100 h-100">
                    <p className="f8 pt3 gray2 w-100 h-100 dtc v-mid tc">
                      Select, create, or join a chat to begin.
                      </p>
                  </div>
                </div>
              </Skeleton>
            );
          }}
        />
        <Route
          exact
          path="/~post/new"
          render={(props) => {
            return (
              <Skeleton
                sidebarHideOnMobile={true}
                sidebar={renderChannelSidebar(props)}
                sidebarShown={state.sidebarShown}
              >
                <NewScreen
                  api={this.api}
                  graphs={state.graphs || {}}
                  {...props}
                />
              </Skeleton>
            );
          }}
        />
        <Route
          exact
          path="/~post/room/:ship/:name"
          render={(props) => {
            let resource =
              `${props.match.params.ship}/${props.match.params.name}`;
            const graph = state.graphs[resource] || new Map();

            return (
              <Skeleton
                sidebarHideOnMobile={true}
                sidebarShown={state.sidebarShown}
                sidebar={renderChannelSidebar(props, resource)}
              >
                <PostScreen
                  resource={{
                    ship: props.match.params.ship,
                    name: props.match.params.name
                  }}
                  api={this.api}
                  subscription={this.subscription}
                  graph={graph}
                  sidebarShown={state.sidebarShown}
                  {...props}
                />
              </Skeleton>
            );
          }}
        />
        <Route
          exact
          path="/~post/room/:ship/:name/:nodeId"
          render={(props) => {
            let resource =
              `${props.match.params.ship}/${props.match.params.name}`;
            const graph = state.graphs[resource] || new Map();
            const node = graph.get(parseInt(props.match.params.nodeId, 10));
            console.log(graph);
            console.log(node);

            return (
              <Skeleton
                sidebarHideOnMobile={true}
                sidebarShown={state.sidebarShown}
                sidebar={renderChannelSidebar(props, resource)}
              >
                <NodeTreeScreen
                  resource={{
                    ship: props.match.params.ship,
                    name: props.match.params.name
                  }}
                  api={this.api}
                  subscription={this.subscription}
                  node={node}
                  sidebarShown={state.sidebarShown}
                  {...props}
                />
              </Skeleton>
            );
          }}
        />
      </div>
  );
  }
}
