import React, { ChangeEvent, useCallback, useMemo } from 'react';

import { Text, Box, BaseInput } from '@tlon/indigo-react';
import { PropFunc } from '~/types';

type StatelessUrlInputProps = PropFunc<typeof BaseInput> & {
  value?: string;
  error?: string;
  focussed?: boolean;
  disabled?: boolean;
  onChange?: (value: string) => void;
  promptUpload: (onError: (err: Error) => void) => Promise<string>;
  canUpload: boolean;
  handleError: (err: Error) => void;
  center?: boolean;
};

export function StatelessUrlInput(props: StatelessUrlInputProps) {
  const {
    value,
    focussed,
    disabled,
    onChange = () => {},
    promptUpload,
    canUpload,
    handleError,
    center = false,
    ...rest
  } = props;

  const placeholder = useMemo(
    () => {
      const centerProps = !center
        ? {}
        : {
            width: '100%',
            height: '100%',
            alignItems: 'center',
            justifyContent: 'center'
          };

      return (
        <Box
          px={2}
          pt={2}
          position="absolute"
          {...centerProps}
          style={{ pointerEvents: 'none' }}
        >
          <Text gray>
            {canUpload ? (
              <>
                Drop or{' '}
                <Text
                  cursor="pointer"
                  color="blue"
                  style={{ pointerEvents: 'all' }}
                  onClick={() => promptUpload(handleError).then(onChange)}
                >
                  upload
                </Text>{' '}
                a file, or paste a URL here
              </>
            ) : (
              'Paste a URL here'
            )}
          </Text>
        </Box>
      );
    },
    [canUpload, promptUpload, onChange, center]
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
