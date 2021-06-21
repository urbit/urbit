import React, { ChangeEvent, useCallback, useMemo } from 'react';

import { Text, Box, BaseInput } from '@tlon/indigo-react';
import { PropFunc } from '~/types';

type StatelessUrlInputProps = PropFunc<typeof BaseInput> & {
  value?: string;
  error?: string;
  focussed?: boolean;
  disabled?: boolean;
  onChange?: (value: string) => void;
  promptUpload: () => Promise<string>;
  canUpload: boolean;
  placeholderOffset?: number | string | number[] | string[];
  leftOffset?: number | string | number[] | string[];
};

export function StatelessUrlInput(props: StatelessUrlInputProps) {
  const {
    value,
    focussed,
    disabled,
    onChange = () => {},
    promptUpload,
    canUpload,
    placeholderOffset = 0,
    leftOffset = 0,
    ...rest
  } = props;

  const placeholder = useMemo(
    () => (
      <Text
        gray
        position="absolute"
        top={placeholderOffset}
        left={leftOffset}
        px={2}
        pt={2}
        style={{ pointerEvents: 'none' }}
      >
        {canUpload ? (
          <>
            Drop or{' '}
            <Text
              cursor="pointer"
              color="blue"
              style={{ pointerEvents: 'all' }}
              onClick={() => promptUpload().then(onChange)}
            >
              upload
            </Text>{' '}
            a file, or paste a link here
          </>
        ) : (
          'Paste a link here'
        )}
      </Text>
    ),
    [canUpload, promptUpload, onChange]
  );

  const handleChange = useCallback(
    (e: ChangeEvent<HTMLInputElement>) => {
      onChange(e.target.value);
    },
    [onChange]
  );

  return (
    <Box position="relative">
      {!(value || focussed || disabled) && placeholder}
      <BaseInput
        type="url"
        p={2}
        width="100%"
        fontSize={1}
        color="black"
        backgroundColor="transparent"
        onChange={handleChange}
        spellCheck="false"
        value={value}
        {...rest}
      />
    </Box>
  );
}
