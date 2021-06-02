import {
  BaseInput,
  Box,
  Button,
  Icon,
  Label,
  Row,
  Text,
} from '@tlon/indigo-react';
import React, { ReactElement, useCallback, useRef } from 'react';
import { useUrlField } from '~/logic/lib/useUrlField';

type ImageInputProps = Parameters<typeof Box>[0] & {
  id: string;
  label?: string;
  placeholder?: string;
};

export const Prompt = ({
  left = null,
  top = null,
  value,
  uploading,
  meta,
  clickUploadButton,
}) => {
  if (!value && !uploading && meta.error === undefined) {
    return (
      <Text
        gray
        fontWeight="500"
        position="absolute"
        left={left}
        top={top}
        style={{ pointerEvents: 'none' }}
      >
        Paste a link here, or{' '}
        <Text
          fontWeight="500"
          cursor="pointer"
          color="blue"
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

export const UploadingStatus = ({ uploading, error }) => {
  if (uploading && rror === undefined) {
    return (
      <Text position="absolute" left={2} top={2} gray>
        Uploading...
      </Text>
    );
  }
  return null;
};

export const ErrorRetry = ({ error, onClick }) => {
  if (error !== undefined) {
    return (
      <Text
        position="absolute"
        left={2}
        top={2}
        color="red"
        style={{ pointerEvents: 'none' }}
      >
        {error}
        {', '}please{' '}
        <Text
          fontWeight="500"
          cursor="pointer"
          color="blue"
          style={{ pointerEvents: 'all' }}
          onClick={onClick}
        >
          retry
        </Text>
      </Text>
    );
  }
  return null;
};

export const ClearButton = ({ value, uploading, onClick, height = null, top = 0 as string | number }) => {
  if (value && !uploading) {
    return (
      <Box
        position="absolute"
        right={0}
        top={top}
        px={1}
        height="100%"
        cursor="pointer"
        onClick={onClick}
        backgroundColor="white"
        display="flex"
        alignItems="center"
        borderRadius="0 4px 4px 0"
        border="1px solid"
        borderColor="lightGray"
      >
        <Icon icon="X" />
      </Box>
    );
  }
  return null;
};

export function ImageInput(props: ImageInputProps): ReactElement {
  const { id, label, caption } = props;
  const ref = useRef<HTMLInputElement>(null);
  const [
    field,
    meta,
    { setValue },
    { uploading, canUpload, onImageUpload },
  ] = useUrlField(id, ref);

  const clickUploadButton = useCallback(() => {
    ref.current?.click();
  }, [ref]);

  const clearEvt = useCallback(() => {
    setValue('');
  }, []);

  return (
    <Box display="flex" flexDirection="column" {...props}>
      {label ? <Label htmlFor={id}>{label}</Label> : null}
      {caption ? (
        <Label mt={2} gray>
          {caption}
        </Label>
      ) : null}
      <Row
        mt={2}
        alignItems="flex-end"
        position="relative"
        height="100%"
        width="100%"
      >
        <Prompt
          left={2}
          top={2}
          value={field.value}
          uploading={uploading}
          meta={meta}
          clickUploadButton={clickUploadButton}
        />
        <ClearButton
          value={field.value}
          uploading={uploading}
          onClick={clearEvt}
        />
        <UploadingStatus uploading={uploading} error={meta.error} />
        <ErrorRetry error={meta.error} onClick={clickUploadButton} />
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
      </Row>
    </Box>
  );
}
