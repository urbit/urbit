import {
  BaseInput, Box,

  Button,

  Icon, Label, Row, StatelessTextInput as Input,

  Text
} from '@tlon/indigo-react';
import { useField } from 'formik';
import React, { ReactElement, useCallback, useRef } from 'react';
import useStorage from '~/logic/lib/useStorage';

type ImageInputProps = Parameters<typeof Box>[0] & {
  id: string;
  label: string;
  placeholder?: string;
};

const prompt = (field, uploading, meta, clickUploadButton) => {
  if (!field.value && !uploading && meta.error === undefined) {
    return (
      <Text
        color='black'
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
  }
  return null;
};

const uploadingStatus = (uploading, meta) => {
  if (uploading && meta.error === undefined) {
    return (
      <Text position='absolute' left={2} top={2} gray>
        Uploading...
      </Text>
    );
  }
  return null;
};

const errorRetry = (meta, uploading, clickUploadButton) => {
  if (meta.error !== undefined) {
    return (
      <Text
        position='absolute'
        left={2}
        top={2}
        color='red'
        style={{ pointerEvents: 'none' }}
      >
        {meta.error}{', '}please{' '}
        <Text
          fontWeight='500'
          cursor='pointer'
          color='blue'
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

  const clearEvt = useCallback(() => {
    setValue('');
  }, []);

  return (
    <Box display="flex" flexDirection="column" className='image-input' {...props}>
      <Label htmlFor={id}>{label}</Label>
      {caption ? (
        <Label mt={2} gray>
          {caption}
        </Label>
      ) : null}
      <Row mt={2} alignItems="flex-end" position='relative' width='100%'>
        {prompt(field, uploading, meta, clickUploadButton)}
        {clearButton(field, uploading, clearEvt)}
        {uploadingStatus(uploading, meta)}
        {errorRetry(meta, uploading, clickUploadButton)}
        <Box background='white' borderRadius={2} width='100%'>
          <Input
            width='100%'
            type={'text'}
            hasError={meta.touched && meta.error !== undefined}
            {...field}
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
