import React, { useCallback } from "react";
import _ from "lodash";
import { Box, ErrorLabel } from "@tlon/indigo-react";
import { useField } from "formik";
import { MarkdownEditor } from "./MarkdownEditor";

export const MarkdownField = ({
  id,
  ...rest
}: { id: string } & Parameters<typeof Box>[0]) => {
  const [{ value, onBlur }, { error, touched }, { setValue }] = useField(id);

  const handleBlur = useCallback(
    (e: any) => {
      _.set(e, "target.id", id);
      console.log(e);
      onBlur && onBlur(e);
    },
    [onBlur, id]
  );

  const hasError = !!(error && touched);

  return (
    <Box
      overflowY="hidden"
      width="100%"
      display="flex"
      flexDirection="column"
      {...rest}
    >
      <MarkdownEditor
        borderColor={hasError ? "red" : "lightGray"}
        onBlur={handleBlur}
        value={value}
        onChange={setValue}
      />
      <ErrorLabel mt="2" hasError={!!(error && touched)}>
        {error}
      </ErrorLabel>
    </Box>
  );
};
