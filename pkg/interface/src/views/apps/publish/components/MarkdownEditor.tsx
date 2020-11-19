import React, { useCallback } from "react";
import { UnControlled as CodeEditor } from "react-codemirror2";
import { useFormikContext } from 'formik';
import { Prompt } from 'react-router-dom';

import { MOBILE_BROWSER_REGEX, usePreventWindowUnload } from "~/logic/lib/util";
import { PropFunc } from "~/types/util";
import CodeMirror from "codemirror";

import "codemirror/mode/markdown/markdown";
import "codemirror/addon/display/placeholder";

import "codemirror/lib/codemirror.css";
import { Box } from "@tlon/indigo-react";

const MARKDOWN_CONFIG = {
  name: "markdown",
};

interface MarkdownEditorProps {
  placeholder?: string;
  value: string;
  onChange: (s: string) => void;
  onBlur?: (e: any) => void;
}

const PromptIfDirty = () => {
  const formik = useFormikContext();
  usePreventWindowUnload(formik.dirty && !formik.isSubmitting);
  return (
    <Prompt
      when={formik.dirty && !formik.isSubmitting}
      message="Are you sure you want to leave? You have unsaved changes."
    />
  );
};

export function MarkdownEditor(
  props: MarkdownEditorProps & PropFunc<typeof Box>
) {
  const { onBlur, placeholder, value, onChange, ...boxProps } = props;

  const options = {
    mode: MARKDOWN_CONFIG,
    theme: "tlon",
    lineNumbers: false,
    lineWrapping: true,
    scrollbarStyle: "native",
    // cursorHeight: 0.85,
    placeholder: placeholder || "",
  };

  const handleChange = useCallback(
    (_e, _d, v: string) => {
      onChange(v);
    },
    [onChange]
  );

  const handleBlur = useCallback(
    (_i, e: any) => {
      onBlur && onBlur(e);
    },
    [onBlur]
  );

  return (
    <Box
      height="100%"
      position="static"
      className="publish"
      p={1}
      border={1}
      borderColor="lightGray"
      borderRadius={2}
      height={['calc(100% - 22vh)', '100%']}
      {...boxProps}
    >
      <PromptIfDirty />
      <CodeEditor
        autoCursor={false}
        onBlur={onBlur}
        value={value}
        options={options}
        onChange={handleChange}
      />
    </Box>
  );
}
