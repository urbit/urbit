import React, { useCallback } from "react";

import { UnControlled as CodeEditor } from "react-codemirror2";
import { MOBILE_BROWSER_REGEX } from "~/logic/lib/util";
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
      flexGrow={1}
      position="static"
      className="publish"
      p={1}
      border={1}
      borderColor="lightGray"
      borderRadius={2}
      {...boxProps}
    >
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
