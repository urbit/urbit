import React from 'react';
import { Box } from '@tlon/indigo-react';

import GlobalApi from '~/logic/api/global';
import { StoreState } from '~/logic/store/type';
import { Association } from '@urbit/api';
import { NotebookRoutes } from './components/NotebookRoutes';

type PublishResourceProps = {
  association: Association;
  api: GlobalApi;
  baseUrl: string;
};

export function PublishResource(props: PublishResourceProps) {
  const { association, api, baseUrl } = props;
  const rid = association.resource;
  const [, , ship, book] = rid.split('/');

  return (
    <Box height="100%" width="100%" overflowY="auto">
      <NotebookRoutes
        api={api}
        association={association}
        rootUrl={baseUrl}
        baseUrl={`${baseUrl}/resource/publish/ship/${ship}/${book}`}
      />
    </Box>
  );
}
