import React, { useCallback } from "react";
import styled from "styled-components";
import { Box, ErrorLabel } from "@tlon/indigo-react";
import { useField } from "formik";
import { MarkdownEditor } from "~/views/components/MarkdownEditor";

export const MarkdownField = ({
  id,
  ...rest
}: { id: string } & Parameters<typeof Box>[0]) => {
  const [{ value }, { error, touched }, { setValue, setTouched }] = useField(
    id
  );

  const onChange = useCallback(
    (s: string) => {
      setValue(s);
      setTouched(true);
    },
    [setValue, setTouched]
  );

  return (
    <Box
      border={1}
      borderRadius={1}
      borderColor="washedGray"
      overflowY="hidden"
      width="100%"
      display="flex"
      flexDirection="column"
      {...rest}
    >
      <MarkdownEditor content={value} onChange={onChange} />
      <ErrorLabel>{touched && error}</ErrorLabel>
    </Box>
  );
};
