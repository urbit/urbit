import React, { Component } from 'react';
import { UnControlled as CodeEditor } from 'react-codemirror2';
import { MOBILE_BROWSER_REGEX } from "~/logic/lib/util";
import CodeMirror from 'codemirror';

import 'codemirror/mode/markdown/markdown';
import 'codemirror/addon/display/placeholder';

import 'codemirror/lib/codemirror.css';

import '../css/custom.css';

const BROWSER_REGEX =
  new RegExp(String(/Android|webOS|iPhone|iPad|iPod|BlackBerry/i));


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

export default class ChatEditor extends Component {
  constructor(props) {
    super(props);

    this.state = {
      message: props.message
    };
    this.editor = null;
  }

  componentWillUnmount() {
    this.props.onUnmount(this.state.message);
  }

  componentDidUpdate(prevProps) {
    const { props } = this;

    if (prevProps.message !== props.message) {
      this.editor.setValue(props.message);
      this.editor.setOption('mode', MARKDOWN_CONFIG);
      return;
    }

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

    this.setState({ message: '' });
    this.props.submit(editorMessage);
    this.editor.setValue('');
  }

  messageChange(editor, data, value) {
    if (this.state.message !== '' && value == '') {
      this.setState({
        message: value
      });
    }
    if (value == this.props.message || value == '' || value == ' ') {
      return;
    }
    this.setState({
      message: value
    });
  }

  render() {
    const {
      inCodeMode,
      placeholder,
      message,
      ...props
    } = this.props;

    const codeTheme = inCodeMode ? ' code' : '';

    const options = {
      mode: MARKDOWN_CONFIG,
      theme: 'tlon' + codeTheme,
      lineNumbers: false,
      lineWrapping: true,
      scrollbarStyle: 'native',
      cursorHeight: 0.85,
      placeholder: inCodeMode ? 'Code...' : placeholder,
      extraKeys: {
        'Enter': () => {
          this.submit();
        },
        'Esc': () => {
          this.editor?.getInputField().blur();
        }
      }
    };

    return (
      <div
        className={
          'chat fr flex h-100 bg-gray0-d lh-copy w-100 items-center ' +
          (inCodeMode ? ' code' : '')
        }
        style={{ flexGrow: 1, paddingTop: '3px', maxHeight: '224px', width: 'calc(100% - 88px)' }}>
        <CodeEditor
          value={message}
          options={options}
          onChange={(e, d, v) => this.messageChange(e, d, v)}
          editorDidMount={(editor) => {
            this.editor = editor;
            if (!MOBILE_BROWSER_REGEX.test(navigator.userAgent)) {
              editor.focus();
            }
          }}
          {...props}
        />
      </div>
    );
  }
}
