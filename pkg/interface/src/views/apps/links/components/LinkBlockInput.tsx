import React, { useCallback, useState } from 'react';
import { Box, LoadingSpinner, Action, Row } from '@tlon/indigo-react';

import { StatelessUrlInput } from '~/views/components/StatelessUrlInput';
import SubmitDragger from '~/views/components/SubmitDragger';
import { Association, resourceFromPath, createPost } from '@urbit/api';
import { parsePermalink, permalinkToReference } from '~/logic/lib/permalinks';
import useGraphState, { GraphState } from '~/logic/state/graph';
import { useFileUpload } from '~/logic/lib/useFileUpload';

interface LinkBlockInputProps {
  size: string;
  url?: string;
  association: Association;
}

const selGraph = (s: GraphState) => s.addPost;

export function LinkBlockInput(props: LinkBlockInputProps) {
  const { size, association } = props;
  const [url, setUrl] = useState(props.url || '');
  const [valid, setValid] = useState(false);
  const [focussed, setFocussed] = useState(false);

  const addPost = useGraphState(selGraph);

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
    setValid(URLparser.test(val) || Boolean(parsePermalink(val)));
  }, []);

  const { uploading, canUpload, promptUpload, drag } = useFileUpload({
    onSuccess: handleChange
  });

  const doPost = () => {
    const text = '';
    const { ship, name } = resourceFromPath(association.resource);
    if (!(valid || url)) {
      return;
    }
    const contents = url.startsWith('web+urbitgraph:/')
      ? [{ text }, permalinkToReference(parsePermalink(url)!)]
      : [{ text }, { url }];

    const post = createPost(window.ship, contents);

    addPost(ship, name, post).then(() => {
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
      backgroundColor="washedGray"
      {...drag.bind}
    >
      {drag.dragging && <SubmitDragger />}
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
          center
        />
      )}
      <Row
        position="absolute"
        right="0"
        bottom="0"
        p="2"
        justifyContent="row-end"
      >
        <Action onClick={doPost} disabled={!valid} backgroundColor="transparent">
          Post
        </Action>
      </Row>
    </Box>
  );
}
