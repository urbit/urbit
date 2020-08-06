import React, { Component } from 'react';
import _ from 'lodash';
import moment from 'moment';
import { UnControlled as CodeEditor } from 'react-codemirror2';
import CodeMirror from 'codemirror';

import 'codemirror/mode/markdown/markdown';
import 'codemirror/addon/display/placeholder';

import 'codemirror/lib/codemirror.css';

import { Sigil } from '../../../../lib/sigil';
import { ShipSearch } from './ship-search';
import { S3Upload } from './s3-upload';

import { uxToHex } from '../../../../lib/util';

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

export class ChatInput extends Component {
  constructor(props) {
    super(props);

    this.state = {
      message: props.message,
      patpSearch: null
    };

    this.textareaRef = React.createRef();

    this.messageSubmit = this.messageSubmit.bind(this);
    this.messageChange = this.messageChange.bind(this);

    this.patpAutocomplete = this.patpAutocomplete.bind(this);
    this.completePatp = this.completePatp.bind(this);
    this.clearSearch = this.clearSearch.bind(this);

    this.toggleCode = this.toggleCode.bind(this);

    this.editor = null;

    moment.updateLocale('en', {
        relativeTime : {
            past: function(input) {
              return input === 'just now'
                ? input
                : input + ' ago';
            },
            s  : 'just now',
            future: 'in %s',
            ss : '%d sec',
            m:  'a minute',
            mm: '%d min',
            h:  'an hr',
            hh: '%d hrs',
            d:  'a day',
            dd: '%d days',
            M:  'a month',
            MM: '%d months',
            y:  'a year',
            yy: '%d years'
        }
    });
  }

  componentWillUnmount() {
    this.props.onUnmount(this.state.message);
  }

  nextAutocompleteSuggestion(backward = false) {
    const { patpSuggestions } = this.state;
    let idx = patpSuggestions.findIndex(s => s === this.state.selectedSuggestion);

    idx = backward ? idx - 1 : idx + 1;
    idx = idx % patpSuggestions.length;
    if(idx < 0) {
      idx = patpSuggestions.length - 1;
    }

    this.setState({ selectedSuggestion: patpSuggestions[idx] });
  }

  patpAutocomplete(message) {
    const match = /~([a-zA-Z\-]*)$/.exec(message);

    if (!match ) {
      this.setState({ patpSearch: null });
      return;
    }
    this.setState({ patpSearch: match[1].toLowerCase() });
  }

  clearSearch() {
    this.setState({
      patpSearch: null
    });
  }

  completePatp(suggestion) {
    if(!this.editor) {
      return;
    }
    const newMessage = this.editor.getValue().replace(
        /[a-zA-Z\-]*$/,
        suggestion
      );
    this.editor.setValue(newMessage);
    const lastRow = this.editor.lastLine();
    const lastCol = this.editor.getLineHandle(lastRow).text.length;
    this.editor.setCursor(lastRow, lastCol);
    this.setState({
      patpSearch: null
    });
  }

  messageChange(editor, data, value) {
    const { patpSearch } = this.state;
    if(patpSearch !== null) {
      this.patpAutocomplete(value, false);
    }
    this.setState({
      message: value
    });
  }

  getLetterType(letter) {
    if (letter.startsWith('/me ')) {
      letter = letter.slice(4);
      // remove insignificant leading whitespace.
      // aces might be relevant to style.
      while (letter[0] === '\n') {
        letter = letter.slice(1);
      }

      return {
        me: letter
      };
    } else if (this.isUrl(letter)) {
       return {
        url: letter
      };
    } else {
      return {
        text: letter
      };
    }
  }

  isUrl(string) {
    try {
      const websiteTest = new RegExp(String(/^((\w+:\/\/)[-a-zA-Z0-9:@;?&=\/%\+\.\*!'\(\),\$_\{\}\^~\[\]`#|]+)/.source)
      );
      return websiteTest.test(string);
    } catch (e) {
      return false;
    }
  }

  messageSubmit() {
    if(!this.editor) {
      return;
    }
    const { props, state } = this;
    const editorMessage = this.editor.getValue();

    if (editorMessage === '') {
      return;
    }

    props.onEnter();

    if(state.code) {
      props.api.chat.message(props.station, `~${window.ship}`, Date.now(), {
        code: {
          expression: editorMessage,
          output: undefined
        }
      });
      this.editor.setValue('');
      return;
    }

    let messages = []; // Users can send one or more messages on submit, depending on message content
    let message = [];
    let isInCodeBlock = false;
    let endOfCodeBlock = false;
    editorMessage.split(/\r?\n/).forEach((line) => {
      message.push('\n');
      // A line of backticks enters and exits a codeblock
      if (line.startsWith('```')) {
        // But we need to check if we've ended a codeblock
        endOfCodeBlock = isInCodeBlock;
        isInCodeBlock = (!isInCodeBlock);
      } else {
        endOfCodeBlock = false;
      }

      if (isInCodeBlock || endOfCodeBlock) {
        message.push(line);
      } else {
        line.split(/\s/).forEach((str) => {
          if (
            (str.startsWith('`') && str !== '`')
            || (str === '`' && !isInCodeBlock)
          ) {
            isInCodeBlock = true;
          } else if (
            (str.endsWith('`') && str !== '`')
            || (str === '`' && isInCodeBlock)
          ) {
            isInCodeBlock = false;
          }

          if (this.isUrl(str) && !isInCodeBlock) {
            if (message.length > 0) {
              // If we're in the middle of a message, add it to the stack and reset
              messages.push(message);
              message = [];
            }
            messages.push([str]);
            message = [];
          } else {
            message.push(str);
          }
        });
      }
    });

    if (message.length) {
      // Add any remaining message
      messages.push(message);
    }

    messages.forEach((message) => {
      if (message.length > 0) {
        message = this.getLetterType(message.join(' '));
        props.api.chat.message(
          props.station,
          `~${window.ship}`,
          Date.now(),
          message
        );
      }
    });

    // perf testing:
    /*let closure = () => {
      let x = 0;
      for (var i = 0; i < 30; i++) {
        x++;
        props.api.chat.message(
          props.station,
          `~${window.ship}`,
          Date.now(),
          {
            text: `${x}`
          }
        );
      }
      setTimeout(closure, 1000);
    };
    this.closure = closure.bind(this);
    setTimeout(this.closure, 2000);*/

    this.editor.setValue('');
  }

  toggleCode() {
    if(this.state.code) {
      this.setState({ code: false });
      this.editor.setOption('mode', MARKDOWN_CONFIG);
      this.editor.setOption('placeholder', this.props.placeholder);
    } else {
      this.setState({ code: true });
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

  uploadSuccess(url) {
    const { props } = this;
    props.api.chat.message(
      props.station,
      `~${window.ship}`,
      Date.now(),
      { url }
    );
  }

  uploadError(error) {
    //  no-op for now
  }

  render() {
    const { props, state } = this;

    const color = props.ownerContact
      ? uxToHex(props.ownerContact.color) : '000000';

    const sigilClass = props.ownerContact
      ? '' : 'mix-blend-diff';

    const img = (props.ownerContact && (props.ownerContact.avatar !== null))
      ? <img src={props.ownerContact.avatar} height={24} width={24} className="dib" />
      : <Sigil
        ship={window.ship}
        size={24}
        color={`#${color}`}
        classes={sigilClass}
        />;

    const candidates = _.chain(this.props.envelopes)
      .defaultTo([])
      .map('author')
      .uniq()
      .reverse()
      .value();

    const codeTheme = state.code ? ' code' : '';

    const options = {
      mode: MARKDOWN_CONFIG,
      theme: 'tlon' + codeTheme,
      lineNumbers: false,
      lineWrapping: true,
      scrollbarStyle: 'native',
      cursorHeight: 0.85,
      placeholder: state.code ? 'Code...' : props.placeholder,
      extraKeys: {
        Tab: cm =>
          this.patpAutocomplete(cm.getValue(), true),
        'Enter': () => {
          this.messageSubmit();
          if (this.state.code) {
            this.toggleCode();
          }
        },
        'Shift-3': cm =>
          cm.getValue().length === 0
            ? this.toggleCode()
            : CodeMirror.Pass
      }
    };

    return (
      <div className="chat pa3 cf flex black white-d bt b--gray4 b--gray1-d bg-white bg-gray0-d relative"
      style={{ flexGrow: 1 }}
      >
        <ShipSearch
          popover
          onSelect={this.completePatp}
          onClear={this.clearSearch}
          contacts={props.contacts}
          candidates={candidates}
          searchTerm={this.state.patpSearch}
          cm={this.editor}
        />
        <div
          className="fl"
          style={{
            marginTop: 6,
            flexBasis: 24,
            height: 24
          }}
        >
        {img}
        </div>
        <div
          className="fr h-100 flex bg-gray0-d lh-copy pl2 w-100 items-center"
          style={{ flexGrow: 1, maxHeight: '224px', width: 'calc(100% - 72px)' }}
        >
          <CodeEditor
            value={this.props.message}
            options={options}
            editorDidMount={(editor) => {
            this.editor = editor;
            if (!/Android|webOS|iPhone|iPad|iPod|BlackBerry/i.test(
                navigator.userAgent
              )) {
              editor.focus();
              }
            }}
            onChange={(e, d, v) => this.messageChange(e, d, v)}
          />
        </div>
        <div className="ml2 mr2"
          style={{ height: '16px', width: '16px', flexBasis: 16, marginTop: 10 }}>
          <S3Upload
            configuration={props.s3.configuration}
            credentials={props.s3.credentials}
            uploadSuccess={this.uploadSuccess.bind(this)}
            uploadError={this.uploadError.bind(this)}
          />
        </div>
        <div style={{ height: '16px', width: '16px', flexBasis: 16, marginTop: 10 }}>
          <img
            style={{ filter: state.code && 'invert(100%)', height: '14px', width: '14px' }}
            onClick={this.toggleCode}
            src="/~chat/img/CodeEval.png"
            className="contrast-10-d bg-white bg-none-d ba b--gray1-d br1"
          />
        </div>
      </div>
    );
  }
}
