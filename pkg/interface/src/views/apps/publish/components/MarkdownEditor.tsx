import React, { createRef, useCallback, useRef } from 'react';
import { IUnControlledCodeMirror, UnControlled as CodeEditor } from 'react-codemirror2';
import { useFormikContext } from 'formik';
import { Prompt } from 'react-router-dom';
import { Editor } from 'codemirror';

import { MOBILE_BROWSER_REGEX, usePreventWindowUnload } from '~/logic/lib/util';
import { PropFunc } from '~/types/util';
import CodeMirror from 'codemirror';

import 'codemirror/mode/markdown/markdown';
import 'codemirror/addon/display/placeholder';
import 'codemirror/addon/edit/continuelist';

import 'codemirror/lib/codemirror.css';
import { Box } from '@tlon/indigo-react';
import { useFileDrag } from '~/logic/lib/useDrag';
import SubmitDragger from '~/views/components/SubmitDragger';
import useStorage from '~/logic/lib/useStorage';
import { StorageState } from '~/types';

const MARKDOWN_CONFIG = {
  name: 'markdown'
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
    theme: 'tlon',
    lineNumbers: false,
    lineWrapping: true,
    scrollbarStyle: 'native',
    // cursorHeight: 0.85,
    placeholder: placeholder || '',
    extraKeys: { 'Enter': 'newlineAndIndentContinueMarkdownList' }
  };

  const editor: React.RefObject<any> = useRef();

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

  const { uploadDefault, canUpload } = useStorage();

  const onFileDrag = useCallback(
    async (files: FileList | File[], e: DragEvent) => {
      if (!canUpload || !editor.current) {
        return;
      }
      const codeMirror: Editor = editor.current.editor;
      const doc = codeMirror.getDoc();

      Array.from(files).forEach(async (file) => {
        const placeholder = `![Uploading ${file.name}](...)`;
        doc.setValue(doc.getValue() + placeholder);
        const url = await uploadDefault(file);
        const markdown = `![${file.name}](${url})`;
        doc.setValue(doc.getValue().replace(placeholder, markdown));
      });
    },
    [uploadDefault, canUpload, value, onChange]
  );

  const { bind, dragging } = useFileDrag(onFileDrag);

  return (
    <Box
      height="100%"
      position="relative"
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
        ref={editor}
        autoCursor={false}
        onBlur={onBlur}
        value={value}
        options={options}
        onChange={handleChange}
        onDragLeave={(editor, e) => bind.onDragLeave(e)}
        onDragOver={(editor, e) => bind.onDragOver(e)}
        onDrop={(editor, e) => bind.onDrop(e)}
        onDragEnter={(editor, e) => bind.onDragEnter(e)}
      />
      {dragging && <SubmitDragger />}
    </Box>
  );
}
