import { Association, Graph, GraphNode, Group } from '@urbit/api';
import bigInt from 'big-integer';
import React from 'react';
import { Route, RouteComponentProps, Switch } from 'react-router-dom';
import GlobalApi from '~/logic/api/global';
import { EditPost } from './EditPost';
import Note from './Note';

interface NoteRoutesProps {
  ship: string;
  book: string;
  note: GraphNode;
  noteId: bigInt.BigInteger;
  notebook: Graph;
  api: GlobalApi;
  association: Association;
  baseUrl?: string;
  rootUrl?: string;
  group: Group
}

export function NoteRoutes(props: NoteRoutesProps & RouteComponentProps) {
  const baseUrl = props.baseUrl || '/~404';
  const rootUrl = props.rootUrl || '/~404';

  const relativePath = (path: string) => `${baseUrl}${path}`;
  return (
    <Switch>
      <Route
        exact
        path={relativePath('/edit')}
        render={routeProps => <EditPost {...routeProps} {...props} />}
      />
      <Route
        path={relativePath('/:commentId?')}
        render={routeProps =>
          <Note
            baseUrl={baseUrl}
            {...props}
            {...routeProps}
            rootUrl={rootUrl}
          />
        }
      />
    </Switch>
  );
}
