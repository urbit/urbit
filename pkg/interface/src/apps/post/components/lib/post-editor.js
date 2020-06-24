import React, { Component } from 'react';
import { UnControlled as CodeEditor } from 'react-codemirror2';
import CodeMirror from 'codemirror';

import 'codemirror/mode/markdown/markdown';
import 'codemirror/addon/display/placeholder';

import 'codemirror/lib/codemirror.css';

const BROWSER_REGEX =
  new RegExp(String(!/Android|webOS|iPhone|iPad|iPod|BlackBerry/i));


const MARKDOWN_CONFIG = {
  name: 'markdown',
  tokenTypeOverrides: {
    header: 'presentation',
    quote: 'presentation',
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

export default class PostEditor extends Component {
  constructor(props) {
    super(props);
    this.editor = null;
  }

  componentDidUpdate(prevProps) {
    const { props } = this;
    if (!props.inCodeMode) {
      this.editor.setOption('mode', MARKDOWN_CONFIG);
      this.editor.setOption('placeholder', this.props.placeholder);
    } else {
      this.editor.setOption('mode', null);
      this.editor.setOption('placeholder', 'Code...');
    }
    const value = this.editor.getValue();

    // Force redraw of placeholder
    if(value.length === 0) {
      this.editor.setValue(' ');
      this.editor.setValue('');
    }
  }

  submit() {
    if(!this.editor) {
      return;
    }

    let editorMessage = this.editor.getValue();
    if (editorMessage === '') {
      return;
    }

    this.props.submit(editorMessage);
    this.editor.setValue('');
  }

  render() {
    const { props } = this;

    const codeTheme = props.inCodeMode ? ' code' : '';

    const options = {
      mode: MARKDOWN_CONFIG,
      theme: 'tlon' + codeTheme,
      lineNumbers: false,
      lineWrapping: true,
      scrollbarStyle: 'native',
      cursorHeight: 0.85,
      placeholder: props.inCodeMode ? 'Code...' : props.placeholder,
      extraKeys: {
        'Enter': () => {
          this.submit();
        }
      }
    };

    return (
      <div
        className="chat fr h-100 flex bg-gray0-d lh-copy pl2 w-100 items-center"
        style={{ flexGrow: 1, maxHeight: '224px', width: 'calc(100% - 72px)' }}
      >
        <CodeEditor
          options={options}
          editorDidMount={(editor) => {
            this.editor = editor;
            if (BROWSER_REGEX.test(navigator.userAgent)) {
              editor.focus();
            }
          }}
        />
      </div>
    );
  }
}
