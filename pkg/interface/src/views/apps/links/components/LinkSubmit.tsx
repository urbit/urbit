import { BaseInput, Box, Button, LoadingSpinner } from '@tlon/indigo-react';
import { hasProvider } from 'oembed-parser';
import React, { useState, useEffect } from 'react';
import { parsePermalink, permalinkToReference } from '~/logic/lib/permalinks';
import { StatelessUrlInput } from '~/views/components/StatelessUrlInput';
import SubmitDragger from '~/views/components/SubmitDragger';
import useGraphState from '~/logic/state/graph';
import { createPost } from '@urbit/api';
import { useFileUpload } from '~/logic/lib/useFileUpload';

interface LinkSubmitProps {
  name: string;
  ship: string;
  parentIndex?: any;
}

const LinkSubmit = (props: LinkSubmitProps) => {
  const addPost = useGraphState(s => s.addPost);

  const [submitFocused, setSubmitFocused] = useState(false);
  const [linkValue, setLinkValue] = useState('');
  const [linkTitle, setLinkTitle] = useState('');
  const [disabled, setDisabled] = useState(false);
  const [linkValid, setLinkValid] = useState(false);

  const {
    canUpload,
    uploading,
    promptUpload,
    drag,
    onPaste
  } = useFileUpload({
    onSuccess: setLinkValue,
    multiple: false
  });

  const doPost = () => {
    const url = linkValue;
    const text = linkTitle ? linkTitle : linkValue;
    const contents = url.startsWith('web+urbitgraph:/')
      ?  [{ text }, permalinkToReference(parsePermalink(url)!)]
      :  [{ text }, { url }];

    setDisabled(true);
    const parentIndex = props.parentIndex || '';
    const post = createPost(window.ship, contents, parentIndex);

    addPost(
      `~${props.ship}`,
      props.name,
      post
    );
    setDisabled(false);
    setLinkValue('');
    setLinkTitle('');
    setLinkValid(false);
  };

  const validateLink = (link) => {
    const URLparser = new RegExp(
      /((?:([\w\d\.-]+)\:\/\/?){1}(?:(www)\.?){0,1}(((?:[\w\d-]+\.)*)([\w\d-]+\.[\w\d]+))){1}(?:\:(\d+)){0,1}((\/(?:(?:[^\/\s\?]+\/)*))(?:([^\?\/\s#]+?(?:.[^\?\s]+){0,1}){0,1}(?:\?([^\s#]+)){0,1})){0,1}(?:#([^#\s]+)){0,1}/
    );

    let linkValid = URLparser.test(link);

    if (!linkValid) {
      linkValid = URLparser.test(`http://${link}`);
      if (linkValid) {
        link = `http://${link}`;
        setLinkValue(link);
      }
    }
    if(link.startsWith('web+urbitgraph://')) {
      const permalink = parsePermalink(link);
      if(!permalink) {
        setLinkValid(false);
        return;
      }
    }

    if (linkValid) {
      if (hasProvider(linkValue)) {
        fetch(`https://noembed.com/embed?url=${linkValue}`)
          .then(response => response.json())
          .then((result) => {
            if (result.title && !linkTitle) {
              setLinkTitle(result.title);
            }
          }).catch((error) => { /* noop*/ });
      } else if (!linkTitle) {
        setLinkTitle(decodeURIComponent(link
          .split('/')
          .pop()
          .split('.')
          .slice(0, -1)
          .join('.')
          .replace('_', ' ')
          .replace(/\d{4}\.\d{1,2}\.\d{2}\.\.\d{2}\.\d{2}\.\d{2}-/, '')
        ));
      }
    }
    return link;
  };

  useEffect(() => {
    setLinkValid(validateLink(linkValue));
  }, [linkValue]);

  const onLinkChange = () => {
    const link = validateLink(linkValue);
    setLinkValid(link);
  };

  useEffect(onLinkChange, [linkValue]);

  const onKeyPress = (e) => {
    if (e.key === 'Enter') {
      e.preventDefault();
      doPost();
    }
  };

  return (
    <>
    {/* @ts-ignore archaic event type mismatch */}
      <Box
        flexShrink={0}
        position='relative'
        border='1px solid'
        borderColor={submitFocused ? 'black' : 'lightGray'}
        width='100%'
        borderRadius={2}
        {...drag.bind}
      >
        {uploading && <Box
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
        </Box>}
      {drag.dragging && <SubmitDragger />}
      <StatelessUrlInput
        value={linkValue}
        promptUpload={promptUpload}
        canUpload={canUpload}
        onSubmit={doPost}
        onChange={setLinkValue}
        error={linkValid ? 'Invalid URL' : undefined}
        onKeyPress={onKeyPress}
        onPaste={onPaste}
      />
        <BaseInput
          type="text"
          pl={2}
          backgroundColor="transparent"
          width="100%"
          color="black"
          fontSize={1}
          style={{
            resize: 'none',
            height: 40
          }}
          placeholder="Provide a title"
          onChange={e => setLinkTitle(e.target.value)}
          onBlur={() => setSubmitFocused(false)}
          onFocus={() => setSubmitFocused(true)}
          spellCheck="false"
          onKeyPress={onKeyPress}
          value={linkTitle}
        />
      </Box>
      <Box mt={2} mb={4}>
        <Button
          primary
          flexShrink={0}
          disabled={!linkValid || disabled}
          onClick={doPost}
        >Post link</Button>
      </Box>
    </>
  );
};

export default LinkSubmit;
