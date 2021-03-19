import React, { useEffect } from 'react';
import { RouteComponentProps, Route, Switch } from 'react-router-dom';
import GlobalApi from '~/logic/api/global';
import {
  Association,
  Associations,
  Graphs,
  Groups,
  Contacts,
  Rolodex,
  Unreads,
} from '@urbit/api';
import { Center, LoadingSpinner } from '@tlon/indigo-react';
import { StorageState } from '~/types';
import bigInt from 'big-integer';

import Notebook from './Notebook';
import NewPost from './new-post';
import { NoteRoutes } from './NoteRoutes';
import useGraphState from '~/logic/state/graph';
import useGroupState from '~/logic/state/group';

interface NotebookRoutesProps {
  api: GlobalApi;
  ship: string;
  book: string;
  baseUrl: string;
  rootUrl: string;
  association: Association;
}

export function NotebookRoutes(
  props: NotebookRoutesProps & RouteComponentProps
) {
  const { ship, book, api, baseUrl, rootUrl } = props;

  useEffect(() => {
    ship && book && api.graph.getGraph(ship, book);
  }, [ship, book]);

  const graphs = useGraphState(state => state.graphs);

  const graph = graphs[`${ship.slice(1)}/${book}`];

  const groups = useGroupState(state => state.groups);

  const group = groups?.[props.association?.group];

  const relativePath = (path: string) => `${baseUrl}${path}`;
  return (
    <Switch>
      <Route
        path={baseUrl}
        exact
        render={(routeProps) => {
          if (!graph) {
            return <Center height="100%"><LoadingSpinner /></Center>;
          }
          return <Notebook
            {...props}
            graph={graph}
            association={props.association}
            rootUrl={rootUrl}
            baseUrl={baseUrl}
                 />;
      }}
      />
      <Route
        path={relativePath('/new')}
        render={routeProps => (
          <NewPost
            {...routeProps}
            api={api}
            book={book}
            ship={ship}
            association={props.association}
            graph={graph}
            baseUrl={baseUrl}
          />
        )}
      />
      <Route
        path={relativePath('/note/:noteId')}
        render={(routeProps) => {
          const { noteId } = routeProps.match.params;
          const noteIdNum = bigInt(noteId);

          if(!graph) {
            return <Center height="100%"><LoadingSpinner /></Center>;
          }
          const note = graph.get(noteIdNum);
          if(!note) {
            return <Center height="100%"><LoadingSpinner /></Center>;
          }
          const noteUrl = `${baseUrl}/note/${noteId}`;
          return (
            <NoteRoutes
              rootUrl={baseUrl}
              baseUrl={noteUrl}
              api={api}
              book={book}
              ship={ship}
              note={note}
              notebook={graph}
              noteId={noteIdNum}
              association={props.association}
              group={group}
              {...routeProps}
            />
          );
        }}
      />
    </Switch>
  );
}
