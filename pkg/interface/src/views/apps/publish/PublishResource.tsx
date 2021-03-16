import React from 'react';
import { Box } from '@tlon/indigo-react';

import { StoreState } from '~/logic/store/type';
import { Association } from '@urbit/api';
import { RouteComponentProps } from 'react-router-dom';
import { NotebookRoutes } from './components/NotebookRoutes';

type PublishResourceProps = StoreState & {
  association: Association;
  baseUrl: string;
} & RouteComponentProps;

export function PublishResource(props: PublishResourceProps) {
  const { association, baseUrl } = props;
  const rid = association.resource;
  const [, , ship, book] = rid.split('/');

  return (
    <Box height="100%" width="100%" overflowY="auto">
      <NotebookRoutes
        
        ship={ship}
        book={book}
        association={association}
        rootUrl={baseUrl}
        baseUrl={`${baseUrl}/resource/publish/ship/${ship}/${book}`}
        history={props.history}
        match={props.match}
        location={props.location}
      />
    </Box>
  );
}
