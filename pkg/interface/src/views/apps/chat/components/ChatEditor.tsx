/* eslint-disable max-lines-per-function */
import { BaseTextArea, Box, Row } from '@tlon/indigo-react';
import 'codemirror/addon/display/placeholder';
import 'codemirror/addon/hint/show-hint';
import 'codemirror/lib/codemirror.css';
import 'codemirror/mode/markdown/markdown';
import React, { useRef, ClipboardEvent, useEffect, useImperativeHandle } from 'react';
import { Controlled as CodeEditor } from 'react-codemirror2';
import styled from 'styled-components';
import { MOBILE_BROWSER_REGEX } from '~/logic/lib/util';
import useSettingsState from '~/logic/state/settings';
import '../css/custom.css';
import { useChatStore } from './ChatPane';

export const isMobile = Boolean(MOBILE_BROWSER_REGEX.test(navigator.userAgent));

const MARKDOWN_CONFIG = {
  name: 'markdown',
  tokenTypeOverrides: {
    header: 'presentation',
    quote: 'quote',
    list1: 'presentation',
    list2: 'presentation',
    list3: 'presentation',
    hr: 'presentation',
    image: 'presentation',
    imageAltText: 'presentation',
    imageMarker: 'presentation',
    formatting: 'presentation',
    linkInline: 'presentation',
    linkEmail: 'presentation',
    linkText: 'presentation',
    linkHref: 'presentation'
  }
};

const defaultOptions = {
  mode: MARKDOWN_CONFIG,
  lineNumbers: false,
  lineWrapping: true,
  scrollbarStyle: 'native',
  cursorHeight: 0.85,
  // The below will ony work once codemirror's bug is fixed
  spellcheck: isMobile,
  autocorrect: isMobile,
  autocapitalize: isMobile
};

// Until CodeMirror supports options.inputStyle = 'textarea' on mobile,
// we need to hack this into a regular input that has some funny behaviors
const inputProxy = input => new Proxy(input, {
  get(target, property) {
    if(property === 'focus') {
      return () => {
        target.focus();
      };
    }
    if (property in target) {
      return target[property];
    }
    if (property === 'execCommand') {
      return () => {
        target.setSelectionRange(target.value.length, target.value.length);
        input.blur();
        input.focus();
      };
    }
    if (property === 'setOption') {
      return () => {};
    }
    if (property === 'getValue') {
      return () => target.value;
    }
    if (property === 'setValue') {
      return (val) => {
        target.value = val;
      };
    }
    if (property === 'element') {
      return input;
    }
  }
});

const MobileBox = styled(Box)`
  display: inline-grid;
  vertical-align: center;
  align-items: stretch;
  position: relative;
  justify-content: flex-start;
  width: 100%;

  &:after,
  textarea {
    grid-area: 2 / 1;
    width: auto;
    min-width: 1em;
    font: inherit;
    padding: 0.25em;
    margin: 0;
    resize: none;
    background: none;
    appearance: none;
    border: none;
  }
  &::after {
    content: attr(data-value) ' ';
    visibility: hidden;
    white-space: pre-wrap;
  }
`;

interface ChatEditorProps {
  inCodeMode: boolean;
  placeholder: string;
  submit: (message: string) => void;
  onPaste: (codemirrorInstance, event: ClipboardEvent) => void;
}

export interface CodeMirrorShim {
  setValue: (string) => void;
  setOption: (option: string, property: any) => void;
  focus: () => void;
  execCommand: (string) => void;
  getValue: () => string;
  getInputField: () => HTMLInputElement;
  element: HTMLElement;
}

const ChatEditor = React.forwardRef<CodeMirrorShim, ChatEditorProps>(({ inCodeMode, placeholder, submit, onPaste }, ref) => {
  const editorRef = useRef<CodeMirrorShim>(null);
  useImperativeHandle(ref, () => editorRef.current);
  const editor = editorRef.current;

  const disableSpellcheck = useSettingsState(s => s.calm.disableSpellcheck);

  const {
    message,
    setMessage
  } = useChatStore();

  const onKeyPress = (e: KeyboardEvent, editor: CodeMirrorShim) => {
    if (!editor) {
      return;
    }

    const focusedTag = document.activeElement?.nodeName?.toLowerCase();
    const shouldCapture = !(focusedTag === 'textarea' || focusedTag === 'input' || e.metaKey || e.ctrlKey);
    if(/^[a-z]|[A-Z]$/.test(e.key) && shouldCapture) {
      editor.focus();
    }
    if(e.key === 'Escape') {
      editor.getInputField().blur();
    }
  };

  useEffect(() => {
    const focusListener = (e: KeyboardEvent) => onKeyPress(e, editorRef.current);
    document.addEventListener('keydown', focusListener);

    return () => {
      document.removeEventListener('keydown', focusListener);
    };
  }, []);

  useEffect(() => {
    if (!editor) {
      return;
    }

    if (inCodeMode) {
      editor.setOption('mode', null);
      editor.setOption('placeholder', 'Code...');
    } else {
      editor.setOption('mode', MARKDOWN_CONFIG);
      editor.setOption('placeholder', placeholder);
    }

    // Force redraw of placeholder
    const value = editor.getValue();
    if(value.length === 0) {
      editor.setValue(' ');
      editor.setValue('');
    }
  }, [inCodeMode, placeholder]);

  function messageChange(editor, data, value) {
    if (message !== '' && value == '') {
      setMessage(value);
    }
    if (value == message || value == '' || value == ' ') {
      return;
    }

    setMessage(value);
  }

  const codeTheme = inCodeMode ? ' code' : '';
  const options = {
    ...defaultOptions,
    theme: 'tlon' + codeTheme,
    placeholder: inCodeMode ? 'Code...' : placeholder,
    extraKeys: {
      'Enter': submit,
      'Esc': () => {
        editor?.getInputField().blur();
      }
    }
  };

  return (
    <Row
      backgroundColor='white'
      alignItems='center'
      flexGrow={1}
      height='100%'
      paddingTop={MOBILE_BROWSER_REGEX.test(navigator.userAgent) ? '16px' : '0'}
      paddingBottom={MOBILE_BROWSER_REGEX.test(navigator.userAgent) ? '16px' : '0'}
      maxHeight='224px'
      width='calc(100% - 88px)'
      className={inCodeMode ? 'chat code' : 'chat'}
      color="black"
      overflow='auto'
    >
      {isMobile
        ? <MobileBox
            data-value={message}
            fontSize={1}
            lineHeight="tall"
            onClick={(event) => {
              if (editor) {
                editor.element.focus();
              }
            }}
          >
            <BaseTextArea
              fontFamily={inCodeMode ? 'Source Code Pro' : 'Inter'}
              fontSize={1}
              lineHeight="tall"
              spellCheck={!disableSpellcheck}
              value={message}
              rows={1}
              style={{ width: '100%', background: 'transparent', color: 'currentColor' }}
              placeholder={inCodeMode ? 'Code...' : 'Message...'}
              onChange={event =>
                messageChange(null, null, event.target.value)
              }
              onKeyDown={event =>
                messageChange(null, null, (event.target as any).value)
              }
              onPaste={e => onPaste(null, e)}
              ref={(input) => {
                if (!input)
                  return;
                editorRef.current = inputProxy(input);
              }}
            />
          </MobileBox>
        : <CodeEditor
            className="lh-copy"
            value={message}
            options={options}
            onBeforeChange={(e, d, v) => messageChange(e, d, v)}
            editorDidMount={(codeEditor) => {
              editorRef.current = codeEditor;
            }}
            onPaste={onPaste as any}
          />
      }

    </Row>
  );
});

export default ChatEditor;
