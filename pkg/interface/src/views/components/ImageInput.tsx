import {
  BaseInput, Box,
  Button,
  Icon, Label, Row, StatelessTextInput as Input,
  Text
} from '@tlon/indigo-react';
import { useField } from 'formik';
import React, { ReactElement, useCallback, useRef, useState } from 'react';
import useStorage from '~/logic/lib/useStorage';

export type ImageInputProps = Parameters<typeof Box>[0] & {
  id: string;
  label?: string;
  placeholder?: string;
};

const prompt = (
  field,
  focus,
  uploading,
  meta,
  clickUploadButton,
  canUpload
) => {
  if (
    !focus &&
    !field.value &&
    !uploading &&
    meta.error === undefined
  ) {
    return (
      <Text
        color='black'
        fontWeight='500'
        position='absolute'
        left={2}
        display='flex'
        height='100%'
        alignItems='center'
        lineHeight={1}
        style={{ pointerEvents: 'none' }}
        onSelect={e => e.preventDefault}
      >
        Paste a link here
        {canUpload ? (
          <>
            , or
            <Text
              fontWeight="500"
              cursor="pointer"
              color="blue"
              style={{ pointerEvents: 'all' }}
              mx='0.5ch'
              onClick={clickUploadButton}
            >
              upload
            </Text>
            a file
          </>
        ) : null}
      </Text>
    );
  }
  return null;
};

const uploadingStatus = (uploading, meta) => {
  if (meta.error === undefined && uploading) {
    return (
      <Text
        position="absolute"
        left={2}
        display='flex'
        height='100%'
        alignItems='center'
        lineHeight={1}
        gray
        onSelect={e => e.preventDefault}
      >
        Uploading...
      </Text>
    );
  }
  return null;
};

const errorRetry = (meta, focus, uploading, clickUploadButton) => {
  if (!focus && meta.error !== undefined) {
    return (
      <Text
        position='absolute'
        left={2}
        display='flex'
        height='100%'
        alignItems='center'
        lineHeight={1}
        color='red'
        style={{ pointerEvents: 'none' }}
        onSelect={e => e.preventDefault}
      >
        {meta.error}{', '}please{' '}
        <Text
          fontWeight='500'
          cursor='pointer'
          color='blue'
          mx='0.5ch'
          style={{ pointerEvents: 'all' }}
          onClick={clickUploadButton}
        >
          retry
        </Text>
      </Text>
    );
  }
  return null;
};

const clearButton = (field, uploading, clearEvt) => {
  if (field.value && !uploading) {
    return (
      <Box
        position='absolute'
        right={0}
        top={0}
        px={1}
        height='100%'
        cursor='pointer'
        onClick={clearEvt}
        backgroundColor='white'
        display='flex'
        alignItems='center'
        borderRadius='0 4px 4px 0'
        border='1px solid'
        borderColor='lightGray'
      >
        <Icon icon='X' />
      </Box>
    );
  }
  return null;
};

export function ImageInput(props: ImageInputProps): ReactElement {
  const { id, label, caption } = props;
  const { uploadDefault, canUpload, uploading } = useStorage();
  const [field, meta, { setValue, setError }] = useField(id);
  const [focus, setFocus] = useState(false);
  const ref = useRef<HTMLInputElement | null>(null);

  const clickUploadButton = useCallback(() => {
    ref.current?.click();
  }, [ref]);

  const clearEvt = useCallback(() => {
    setValue('');
  }, []);

  const handleBlur = (e) => {
    field.onBlur(e);
    setFocus(false);
  };

  const onImageUpload = useCallback(async () => {
    const file = ref.current?.files?.item(0);

    if (!file || !canUpload) {
      return;
    }
    try {
      const url = await uploadDefault(file);
      setFocus(false);
      setValue(url);
    } catch (e) {
      setFocus(false);
      setError(e.message);
    }
  }, [ref.current, uploadDefault, canUpload, setValue]);

  return (
    <Box display="flex" flexDirection="column" {...props}>
      <Label htmlFor={id}>{label}</Label>
      {caption ? (
        <Label mt={2} gray>
          {caption}
        </Label>
      ) : null}
      <Row mt={2} alignItems="flex-end" position='relative' width='100%'>
        {prompt(field, focus, uploading, meta, clickUploadButton, canUpload)}
        {clearButton(field, uploading, clearEvt)}
        {uploadingStatus(uploading, meta)}
        {errorRetry(meta, focus, uploading, clickUploadButton)}
        <Box background='white' borderRadius={2} width='100%'>
          <Input
            {...field}
            width='100%'
            type={'text'}
            onFocus={() => setFocus(true)}
            onBlur={e => handleBlur(e)}
            hasError={meta.touched && meta.error !== undefined}
          />
        </Box>
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
    </Box>
  );
}
