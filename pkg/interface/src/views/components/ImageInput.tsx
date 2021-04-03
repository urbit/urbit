import React, { useRef, useCallback, ReactElement } from 'react';
import { useField } from 'formik';

import {
  Box,
  StatelessTextInput as Input,
  Row,
  Button,
  Label,
  ErrorLabel,
  BaseInput,
  Text
} from '@tlon/indigo-react';

import useStorage from '~/logic/lib/useStorage';

type ImageInputProps = Parameters<typeof Box>[0] & {
  id: string;
  label: string;
  placeholder?: string;
};

export function ImageInput(props: ImageInputProps): ReactElement {
  const { id, label, caption } = props;
  const { uploadDefault, canUpload, uploading } = useStorage();
  const [field, meta, { setValue, setError }] = useField(id);
  const ref = useRef<HTMLInputElement | null>(null);

  const onImageUpload = useCallback(async () => {
    const file = ref.current?.files?.item(0);

    if (!file || !canUpload) {
      return;
    }
    try {
      const url = await uploadDefault(file);
      setValue(url);
    } catch (e) {
      setError(e.message);
    }
  }, [ref.current, uploadDefault, canUpload, setValue]);

  const clickUploadButton = useCallback(() => {
    ref.current?.click();
  }, [ref]);

  const Prompt = () => (
    <Text
      black
      fontWeight='500'
      position='absolute'
      left={2}
      top={2}
      style={{ pointerEvents: 'none' }}
    >
      Paste a link here, or{' '}
      <Text
        fontWeight='500'
        cursor='pointer'
        color='blue'
        style={{ pointerEvents: 'all' }}
        onClick={clickUploadButton}
      >
        upload
      </Text>{' '}
      a file
    </Text>
  );

  const Uploading = () => (
    <Text position='absolute' left={2} top={2} gray>
      Uploading...
    </Text>
  );

  const ErrorRetry = () => (
    <Text position='absolute' left={2} top={2} color='red'>
      Error, please{' '}
      <Text
        fontWeight='500'
        cursor='pointer'
        color='blue'
        style={{ pointerEvents: 'all' }}
        onClick={clickUploadButton}
      >
        {' '}
        retry
      </Text>
    </Text>
  );

  return (
    <Box display="flex" flexDirection="column" {...props}>
      <Label htmlFor={id}>{label}</Label>
      {caption ? (
        <Label mt="2" gray>
          {caption}
        </Label>
      ) : null}
      <Row mt="2" alignItems="flex-end" position='relative'>
        {!field.value && !uploading && meta.error === undefined ? (<Prompt />) : null}
        {uploading && meta.error === undefined ? (<Uploading />) : null}
        {meta.touched && meta.error !== undefined ? (<ErrorRetry />) : null}
        <Input
          type={'text'}
          hasError={meta.touched && meta.error !== undefined}
          {...field}
        />
        {canUpload && (
          <>
            <Button
              display='none'
              onClick={clickUploadButton}
            />
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
      </Row>
      <ErrorLabel mt="2" hasError={Boolean(meta.touched && meta.error)}>
        {meta.error}
      </ErrorLabel>
    </Box>
  );
}
