import { Center, Text } from '@tlon/indigo-react';
import React, { ReactElement } from 'react';
import { Route, Switch, useHistory } from 'react-router-dom';
import GlobalApi from '~/logic/api/global';
import { deSig } from '~/logic/lib/util';
import useGraphState from '~/logic/state/graph';
import useMetadataState from '~/logic/state/metadata';

interface GraphAppProps {
  api: GlobalApi;
}

const GraphApp = (props: GraphAppProps): ReactElement => {
  const associations= useMetadataState(state => state.associations);
  const graphKeys = useGraphState(state => state.graphKeys);
  const history = useHistory();

  const { api } = props;

  return (
    <Switch>
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
          } else if(Boolean(association) && 'graph' in association.config) {
            history.push(`/~landscape/home/resource/${association.metadata.config.graph}${path}`);
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
};

export default GraphApp;
