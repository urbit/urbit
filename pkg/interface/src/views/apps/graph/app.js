import React, { Component } from 'react';
import { Switch, Route } from 'react-router-dom';


export default class GraphApp extends Component {
  render() {
    const { props } = this;
    const contacts = props.contacts ? props.contacts : {};
    const groups = props.groups ? props.groups : {};
    const associations =
      props.associations ? props.associations : { graph: {}, contacts: {} };
    const graphKeys = props.graphKeys || new Set([]);
    const graphs = props.graphs || {};

    const { api, sidebarShown, hideAvatars, hideNicknames, s3, remoteContentPolicy } = this.props;

    return (
      <>
        <Switch>
          <Route exact path="/~graph/join/ship/:ship/:name"
            render={ (props) => {
              const resource =
                `${props.match.params.ship}/${props.match.params.name}`;

              const autoJoin = () => {
                try {
                  api.graph.joinGraph(
                    `~${props.match.params.ship}`,
                    props.match.params.name
                  );
                  props.history.push(`/~graph/${resource}`);
                } catch(err) {
                  setTimeout(autoJoin, 2000);
                }
              };
              autoJoin();
            }}
          />
          <Route exact path="/~graph/(popout)?/:ship/:name"
            render={ (props) => {
              const resourcePath = 
                `${props.match.params.ship}/${props.match.params.name}`;
              const resource =
                associations.graph[resourcePath] ?
                  associations.graph[resourcePath] : { metadata: {} };
              const contactDetails = contacts[resource['group-path']] || {};
              const popout = props.match.url.includes('/popout/');
              const graph = graphs[resourcePath] || null;

              if ('app-name' in resource) {
                //  TODO: add proper tags to graph-store's tag-queries,
                //  then push the proper path to history
                props.history.push();
              }

              return (
                <div>Redirecting...</div>
              );
            }}
          />
        </Switch>
      </>
    );
  }
}

