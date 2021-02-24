import React, { useEffect } from 'react';
import { RouteComponentProps, Route, Switch } from 'react-router-dom';
import bigInt from 'big-integer';

import {
  Association,
  Contacts,
  Unreads
} from '@urbit/api';
import { Center, LoadingSpinner } from '@tlon/indigo-react';

import Notebook from './Notebook';
import NewPost from './new-post';
import { NoteRoutes } from './NoteRoutes';
import { S3State } from '~/types';
import useApi from '~/logic/lib/useApi';
import useGraphState from '~/logic/state/graph';
import useGroupState from '~/logic/state/groups';

interface NotebookRoutesProps {
  ship: string;
  book: string;
  unreads: Unreads;
  baseUrl: string;
  rootUrl: string;
  association: Association;
  s3: S3State;
}

export function NotebookRoutes(
  props: NotebookRoutesProps & RouteComponentProps
) {
  const { ship, book, baseUrl, rootUrl } = props;
  const getGraph = useGraphState(state => state.getGraph);
  const groups = useGroupState(state => state.groups);
  const graphs = useGraphState(state => state.graphs);

  useEffect(() => {
    ship && book && getGraph(ship, book);
  }, [ship, book]);

  const graph = graphs[`${ship.slice(1)}/${book}`];

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
            book={book}
            ship={ship}
            association={props.association}
            graph={graph}
            baseUrl={baseUrl}
            s3={props.s3}
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
              book={book}
              ship={ship}
              note={note}
              notebook={graph}
              unreads={props.unreads}
              noteId={noteIdNum}
              association={props.association}
              group={group}
              s3={props.s3}
              {...routeProps}
            />
          );
        }}
      />
    </Switch>
  );
}
