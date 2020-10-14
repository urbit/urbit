import React from 'react';
import styled from 'styled-components';
import { MarkdownEditor as _MarkdownEditor, Box, ErrorMessage } from '@tlon/indigo-react';
import { useField } from 'formik';

const MarkdownEditor = styled(_MarkdownEditor)`
  border: 1px solid ${(p) => p.theme.colors.lightGray};
  border-radius: ${(p) => p.theme.radii[2]}px;
`;

export const MarkdownField = ({ id, ...rest }: { id: string; } & Parameters<typeof Box>[0]) => {
  const [{ value }, { error, touched }, { setValue, setTouched }] = useField(id);

  return (
    <Box overflowY="hidden" width="100%" display="flex" flexDirection="column" {...rest}>
      <MarkdownEditor
        onFocus={() => setTouched(true)}
        onBlur={() => setTouched(false)}
        value={value}
        onBeforeChange={(e, d, v) => setValue(v)}
      />
      <ErrorMessage>{touched && error}</ErrorMessage>
    </Box>
  );
};


