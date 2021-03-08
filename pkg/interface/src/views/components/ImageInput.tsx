import React, { useRef, useCallback, ReactElement } from 'react';
import { useField } from 'formik';

import {
  Box,
  StatelessTextInput as Input,
  Row,
  Button,
  Label,
  ErrorLabel,
  BaseInput
} from '@tlon/indigo-react';

import { StorageState } from '~/types';
import useStorage from '~/logic/lib/useStorage';

type ImageInputProps = Parameters<typeof Box>[0] & {
  id: string;
  label: string;
  placeholder?: string;
};

export function ImageInput(props: ImageInputProps): ReactElement {
  const { id, label, caption, placeholder } = props;

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

  const onClick = useCallback(() => {
    ref.current?.click();
  }, [ref]);

  return (
    <Box display="flex" flexDirection="column" {...props}>
      <Label htmlFor={id}>{label}</Label>
      {caption ? (
        <Label mt="2" gray>
          {caption}
        </Label>
      ) : null}
      <Row mt="2" alignItems="flex-end">
        <Input
          type={'text'}
          hasError={meta.touched && meta.error !== undefined}
          placeholder={placeholder}
          {...field}
        />
        {canUpload && (
          <>
            <Button
              type="button"
              ml={1}
              border={1}
              borderColor="lightGray"
              onClick={onClick}
              flexShrink={0}
            >
              {uploading ? 'Uploading' : 'Upload'}
            </Button>
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
