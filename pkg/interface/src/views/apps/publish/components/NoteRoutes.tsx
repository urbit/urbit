import React from 'react';
import { Route, Switch, RouteComponentProps } from 'react-router-dom';

import { GraphNode, Graph,  Association, Group } from '@urbit/api';
import Note from './Note';
import { EditPost } from './EditPost';
import { S3State } from '~/types';

interface NoteRoutesProps {
  ship: string;
  book: string;
  note: GraphNode;
  noteId: number;
  notebook: Graph;
  association: Association;
  baseUrl?: string;
  rootUrl?: string;
  group: Group;
  s3: S3State;
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
