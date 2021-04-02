import React from 'react';
import { Route, Switch } from 'react-router-dom';

import GlobalApi from '~/logic/api/global';
import { RouteComponentProps } from 'react-router-dom';
import Note from './Note';
import { EditPost } from './EditPost';

import { GraphNode, Graph, Contacts, Association, Group } from '@urbit/api';
import { StorageState } from '~/types';

interface NoteRoutesProps {
  ship: string;
  book: string;
  note: GraphNode;
  noteId: number;
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
