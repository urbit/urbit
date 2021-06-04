import React, { useCallback, useState } from 'react';
import { Box, LoadingSpinner, Action, Row } from '@tlon/indigo-react';

import useStorage from '~/logic/lib/useStorage';
import { StatelessUrlInput } from '~/views/components/StatelessUrlInput';
import { Association, GraphNode, resourceFromPath } from '@urbit/api';
import { createPost } from '~/logic/api/graph';
import { parsePermalink, permalinkToReference } from '~/logic/lib/permalinks';
import GlobalApi from '~/logic/api/global';

interface LinkBlockInputProps {
  size: string;
  url?: string;
  api: GlobalApi;
  association: Association;
}
export function LinkBlockInput(props: LinkBlockInputProps) {
  const { size, association, api } = props;
  const [url, setUrl] = useState(props.url || '');
  const [error, setError] = useState<string | undefined>();
  const [disabled, setDisabled] = useState(false);
  const [valid, setValid] = useState(false);
  const [focussed, setFocussed] = useState(false);

  const { uploading, canUpload, promptUpload } = useStorage();

  const onFocus = useCallback(() => {
    setFocussed(true);
  }, []);

  const onBlur = useCallback(() => {
    setFocussed(false);
  }, []);

  const URLparser = new RegExp(
    /((?:([\w\d\.-]+)\:\/\/?){1}(?:(www)\.?){0,1}(((?:[\w\d-]+\.)*)([\w\d-]+\.[\w\d]+))){1}(?:\:(\d+)){0,1}((\/(?:(?:[^\/\s\?]+\/)*))(?:([^\?\/\s#]+?(?:.[^\?\s]+){0,1}){0,1}(?:\?([^\s#]+)){0,1})){0,1}(?:#([^#\s]+)){0,1}/
  );

  const handleChange = useCallback((val: string) => {
    setUrl(val);
    setValid(URLparser.test(val));
  }, []);

  const doPost = () => {
    const text = '';
    const { ship, name } = resourceFromPath(association.resource);
    if (!(valid || url)) {
      return;
    }
    const contents = url.startsWith('web+urbitgraph:/')
      ? [{ text }, permalinkToReference(parsePermalink(url)!)]
      : [{ text }, { url }];

    setDisabled(true);
    const post = createPost(contents);

    api.graph.addPost(ship, name, post).then(() => {
      setDisabled(false);
      setUrl('');
      setValid(false);
    });
  };

  const onKeyPress = useCallback(
    (e) => {
      if (e.key === 'Enter') {
        e.preventDefault();
        doPost();
      }
    },
    [doPost]
  );

  return (
    <Box
      height={size}
      width={size}
      border="1"
      borderColor="lightGray"
      borderRadius="2"
      alignItems="center"
      display="flex"
      justifyContent="center"
      flexDirection="column"
      p="2"
      position="relative"
    >
      {uploading ? (
        <Box
          display="flex"
          width="100%"
          height="100%"
          position="absolute"
          left={0}
          right={0}
          bg="white"
          zIndex={9}
          alignItems="center"
          justifyContent="center"
        >
          <LoadingSpinner />
        </Box>
      ) : (
        <StatelessUrlInput
          value={url}
          onChange={handleChange}
          canUpload={canUpload}
          onFocus={onFocus}
          focussed={focussed}
          onBlur={onBlur}
          promptUpload={promptUpload}
          onKeyPress={onKeyPress}
          placeholderOffset="-10px"
        />
      )}
      <Row
        position="absolute"
        right="0"
        bottom="0"
        p="2"
        justifyContent="row-end"
      >
        <Action onClick={doPost} disabled={!valid} backgroundColor="white">
          Post
        </Action>
      </Row>
    </Box>
  );
}
