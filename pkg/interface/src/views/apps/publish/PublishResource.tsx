import { Box } from '@tlon/indigo-react';
import { Association } from '@urbit/api';
import React, { useEffect, useRef } from 'react';
import { useLocation } from 'react-router-dom';
import { NotebookRoutes } from './components/NotebookRoutes';

type PublishResourceProps = {
  association: Association;
  baseUrl: string;
  history?: any;
  match?: any;
  location?: any;
};

export function PublishResource(props: PublishResourceProps) {
  const { association, baseUrl } = props;
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
