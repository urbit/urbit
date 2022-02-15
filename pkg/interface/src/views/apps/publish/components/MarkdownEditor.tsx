import { Box } from '@tlon/indigo-react';
import { Editor } from 'codemirror';
import 'codemirror/addon/display/placeholder';
import 'codemirror/addon/edit/continuelist';
import 'codemirror/lib/codemirror.css';
import 'codemirror/mode/markdown/markdown';
import { useFormikContext } from 'formik';
import React, { useCallback, useRef } from 'react';
import { UnControlled as CodeEditor } from 'react-codemirror2';
import { Prompt } from 'react-router-dom';
import { useFileUpload } from '~/logic/lib/useFileUpload';
import { IuseStorage } from '~/logic/lib/useStorage';
import { usePreventWindowUnload } from '~/logic/lib/util';
import { PropFunc } from '~/types/util';
import SubmitDragger from '~/views/components/SubmitDragger';

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
  const { onBlur, placeholder, value, height, onChange, ...boxProps } = props;

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

  const onFileUpload = useCallback(
    async (files: FileList | File[], { canUpload, uploadDefault }: IuseStorage) => {
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
    [value, onChange]
  );

  const {
    drag: { bind, dragging }
  } = useFileUpload({
    onFiles: onFileUpload
  });

  return (
    <Box
      position="relative"
      className="publish"
      p={1}
      border={1}
      borderColor="lightGray"
      borderRadius={2}
      height={height || ['calc(100% - 22vh)', '100%']}
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
        onDragLeave={(editor, e: any) => bind.onDragLeave(e)}
        onDragOver={(editor, e: any) => bind.onDragOver(e)}
        onDrop={(editor, e: any) => bind.onDrop(e)}
        onDragEnter={(editor, e: any) => bind.onDragEnter(e)}
      />
      {dragging && <SubmitDragger />}
    </Box>
  );
}
