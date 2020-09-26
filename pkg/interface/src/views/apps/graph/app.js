import React, { PureComponent } from 'react';
import { Switch, Route } from 'react-router-dom';
import { Center, Text } from "@tlon/indigo-react";
import { deSig } from '~/logic/lib/util';


export default class GraphApp extends PureComponent {
  render() {
    const { props } = this;
    const contacts = props.contacts ? props.contacts : {};
    const groups = props.groups ? props.groups : {};
    const associations =
      props.associations ? props.associations : { graph: {}, contacts: {} };
    const graphKeys = props.graphKeys || new Set([]);
    const graphs = props.graphs || {};

    const {
      api, sidebarShown, s3,
      hideAvatars, hideNicknames, remoteContentPolicy
    } = this.props;

    return (
      <Switch>
        <Route exact path="/~graph/join/ship/:ship/:name/:module?"
          render={ (props) => {
            const resource =
              `${deSig(props.match.params.ship)}/${props.match.params.name}`;

            const autoJoin = () => {
              try {
                api.graph.joinGraph(
                  `~${deSig(props.match.params.ship)}`,
                  props.match.params.name
                );
                
                if (props.match.params.module) {
                  props.history.push(
                    `/~${props.match.params.module}/${resource}`
                  );
                } else {
                  props.history.push('/');
                }
              } catch(err) {
                setTimeout(autoJoin, 2000);
              }
            };

            if(!graphKeys.has(resource)) {
              autoJoin();
            } else if(props.match.params.module) {
              props.history.push(`/~${props.match.params.module}/${resource}`);
            }
            return (
              <Center width="100%" height="100%">
                <Text fontSize={1}>Redirecting...</Text>
              </Center>
            );
          }}
        />
      </Switch>
    );
  }
}

