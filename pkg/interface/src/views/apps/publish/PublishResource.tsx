import { Box } from '@tlon/indigo-react';
import { Association } from '@urbit/api';
import React, { useEffect, useRef } from 'react';
import { useLocation } from 'react-router-dom';
import GlobalApi from '~/logic/api/global';
import { StoreState } from '~/logic/store/type';
import { NotebookRoutes } from './components/NotebookRoutes';

type PublishResourceProps = StoreState & {
  association: Association;
  api: GlobalApi;
  baseUrl: string;
};

export function PublishResource(props: PublishResourceProps) {
  const { association, api, baseUrl, notebooks } = props;
  const rid = association.resource;
  const [, , ship, book] = rid.split('/');
  const location = useLocation();
  const scrollRef = useRef(null);
  useEffect(() => {
    const search = new URLSearchParams(location.search);
    if(search.has('selected') || search.has('edit') || !scrollRef.current) {
      return;
    }
    scrollRef.current.scrollTop = 0;
  }, [location]);

  return (
    <Box ref={scrollRef} height="100%" width="100%" overflowY="auto">
      <NotebookRoutes
        api={api}
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
