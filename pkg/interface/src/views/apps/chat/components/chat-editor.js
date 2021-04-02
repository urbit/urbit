import React, { Component } from 'react';
import { UnControlled as CodeEditor } from 'react-codemirror2';
import { MOBILE_BROWSER_REGEX } from "~/logic/lib/util";
import CodeMirror from 'codemirror';
import styled from "styled-components";

import { Row, BaseTextArea, Box } from '@tlon/indigo-react';

import 'codemirror/mode/markdown/markdown';
import 'codemirror/addon/display/placeholder';
import 'codemirror/addon/hint/show-hint';

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

// Until CodeMirror supports options.inputStyle = 'textarea' on mobile,
// we need to hack this into a regular input that has some funny behaviors
const inputProxy = (input) => new Proxy(input, {
  get(target, property) {
    if (property in target) {
      return target[property];
    }
    if (property === 'setOption') {
      return () => {};
    }
    if (property === 'getValue') {
      return () => target.value;
    }
    if (property === 'setValue') {
      return (val) => target.value = val;
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
      this.editor?.element?.focus();
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
    if(value.endsWith('/')) {
      editor.showHint(['test', 'foo']);
    }
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
      },
      // The below will ony work once codemirror's bug is fixed
      spellcheck: !!MOBILE_BROWSER_REGEX.test(navigator.userAgent),
      autocorrect: !!MOBILE_BROWSER_REGEX.test(navigator.userAgent),
      autocapitalize: !!MOBILE_BROWSER_REGEX.test(navigator.userAgent)
    };

    return (
      <Row
        backgroundColor='white'
        alignItems='center'
        flexGrow='1'
        height='100%'
        paddingTop={MOBILE_BROWSER_REGEX.test(navigator.userAgent) ? '16px' : '0'}
        paddingBottom={MOBILE_BROWSER_REGEX.test(navigator.userAgent) ? '16px' : '0'}
        maxHeight='224px'
        width='calc(100% - 88px)'
        className={inCodeMode ? 'chat code' : 'chat'}
        color="black"
        overflow='auto'
      >
        {MOBILE_BROWSER_REGEX.test(navigator.userAgent)
          ? <MobileBox
              data-value={this.state.message}
              fontSize="1"
              lineHeight="tall"
              onClick={event => {
                if (this.editor) {
                  this.editor.element.focus();
                }
              }}
            >
            <BaseTextArea
              fontFamily={inCodeMode ? 'Source Code Pro' : 'Inter'}
              fontSize="1"
              lineHeight="tall"
              rows="1"
              style={{ width: '100%', background: 'transparent', color: 'currentColor' }}
              placeholder={inCodeMode ? "Code..." : "Message..."}
              onChange={event => {
                this.messageChange(null, null, event.target.value);
              }}
              onKeyDown={event => {
                if (event.key === 'Enter') {
                  event.preventDefault();
                  this.submit();
                } else {
                  this.messageChange(null, null, event.target.value);
                }
              }}
              ref={input => {
                if (!input) return;
                this.editor = inputProxy(input);
              }}
              {...props}
            />
          </MobileBox>
          : <CodeEditor
          className="lh-copy"
          value={message}
          options={options}
          onChange={(e, d, v) => this.messageChange(e, d, v)}
          editorDidMount={(editor) => {
            this.editor = editor;
            editor.focus();
          }}
          {...props}
        />
        }

      </Row>
    );
  }
}
