import React, { ChangeEvent, useCallback, useRef, useState } from 'react';
import { BaseInput, Box, Label, Button } from '@tlon/indigo-react';

import {
  Prompt,
  ClearButton,
  UploadingStatus,
  ErrorRetry,
} from '~/views/components/ImageInput';
import useStorage from '~/logic/lib/useStorage';

interface LinkBlockInputProps {
  size: string;
  label: string;
  caption: string;
  id: string;
  url?: string;
}
export function LinkBlockInput(props: LinkBlockInputProps) {
  const { id, size, label, caption } = props;
  const [url, setUrl] = useState(props.url || '');
  const [error, setError] = useState<string | undefined>();

  const onUrlChange = useCallback((e: ChangeEvent<HTMLInputElement>) => {
    setUrl(e.target.value);
  }, []);

  const ref = useRef<HTMLInputElement>();

  const clickUploadButton = useCallback(() => {
    ref.current?.click();
  }, [ref]);

  const { uploading, canUpload, uploadDefault } = useStorage();

  const clearEvt = useCallback(() => {
    setUrl('');
  }, []);

  const onImageUpload = useCallback(async () => {
    const file = ref.current?.files?.item(0);

    if (!file || !canUpload) {
      return;
    }
    try {
      const url = await uploadDefault(file);
      setUrl(url);
    } catch (e) {
      setError(e.message);
    }
  }, [ref.current, uploadDefault, canUpload]);

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
      {label ? <Label htmlFor={id}>{label}</Label> : null}
      {caption ? (
        <Label mt={2} gray>
          {caption}
        </Label>
      ) : null}
      <BaseInput
        width="100%"
        type={'text'}
        lineHeight="tall"
        backgroundColor="white"
        color="black"
        fontFamily="sans"
        fontWeight="500"
        fontSize="1"
        flexGrow={1}
        onChange={onUrlChange}
        value={url}
      />
      <Prompt
        left={2}
        meta={{} as any}
        uploading={false}
        value={url}
        clickUploadButton={clickUploadButton}
      />
      <UploadingStatus uploading={uploading} error={error} />
      <ErrorRetry error={error} onClick={clickUploadButton} />
      {canUpload && (
        <>
          <Button display="none" onClick={clickUploadButton} />
          <BaseInput
            style={{ display: 'none' }}
            type="file"
            id="fileElement"
            ref={ref}
            accept="image/*"
            onChange={onImageUpload}
          />
        </>
      )}
    </Box>
  );
}
