import React, { useCallback } from 'react';
import _ from 'lodash';
import { Box, ErrorLabel } from '@tlon/indigo-react';
import { useField } from 'formik';
import { MarkdownEditor } from './MarkdownEditor';

export const MarkdownField = ({
  id,
  ...rest
}: { id: string } & Parameters<typeof Box>[0]) => {
  const [{ value, onBlur }, { error, touched }, { setValue }] = useField(id);

  const handleBlur = useCallback(
    (e: any) => {
      _.set(e, 'target.id', id);
      onBlur && onBlur(e);
    },
    [onBlur, id]
  );

  const hasError = Boolean(error && touched);

  return (
    <Box
      overflowY="hidden"
      height='100%'
      width="100%"
      display="flex"
      flexDirection="column"
      color="black"
      {...rest}
    >
      <MarkdownEditor
        borderColor={hasError ? 'red' : 'lightGray'}
        onBlur={handleBlur}
        value={value}
        onChange={setValue}
      />
      <ErrorLabel mt="2" hasError={Boolean(error && touched)}>
        {error}
      </ErrorLabel>
    </Box>
  );
};
