import React, { PureComponent } from 'react';
import { Switch, Route, useHistory } from 'react-router-dom';
import { Center, Text } from "@tlon/indigo-react";
import { deSig } from '~/logic/lib/util';
import useGraphState from '~/logic/state/graph';
import useMetadataState from '~/logic/state/metadata';
import useGroupState from '~/logic/state/group';
import { GraphIndexRoute } from './graphIndex';

const GraphApp = (props) => {
  const associations= useMetadataState(state => state.associations);
  const graphKeys = useGraphState(state => state.graphKeys);
  const groups = useGroupState(state => state.groups);
  const history = useHistory();

  const { api } = props;

  return (
    <Switch>
      <Route path="/~graph/graph/ship/:ship/:name"
        render={(props) => {
          const resource =
            `${deSig(props.match.params.ship)}/${props.match.params.name}`;
          const { ship, name } = props.match.params;
          const path = `/ship/~${deSig(ship)}/${name}`;
          const association = associations.graph[path];
          const url = `/~graph/graph/ship/${ship}/${name}`;
          const group = groups[association.group];
          if(!(group && association)) {
            return null;
          }

          return (
            <GraphIndexRoute url={url} association={association} index="" group={group} />
          );

        }}
      />
      <Route exact path="/~graph/join/ship/:ship/:name/:module?"
        render={(props) => {
          const resource =
            `${deSig(props.match.params.ship)}/${props.match.params.name}`;
          const { ship, name } = props.match.params;
          const path = `/ship/~${deSig(ship)}/${name}`;
          const association = associations.graph[path];


          const autoJoin = () => {
            try {
              api.graph.joinGraph(
                `~${deSig(props.match.params.ship)}`,
                props.match.params.name
              );
              
              
            } catch(err) {
              setTimeout(autoJoin, 2000);
            }
          };

          if(!graphKeys.has(resource)) {
            autoJoin();
          } else if(!!association) {
            history.push(`/~landscape/home/resource/${association.metadata.module}${path}`);
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

export default GraphApp;
