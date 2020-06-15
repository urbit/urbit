import React, { Component } from 'react';
import _ from 'lodash';
import moment from 'moment';
import { UnControlled as CodeEditor } from 'react-codemirror2';
import CodeMirror from 'codemirror';

import 'codemirror/mode/markdown/markdown';
import 'codemirror/addon/display/placeholder';

import 'codemirror/lib/codemirror.css';

import { Sigil } from '../../../../lib/sigil';

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

export class PostInput extends Component {
  constructor(props) {
    super(props);

    this.state = {
      message: '',
    };

    this.textareaRef = React.createRef();

    this.messageSubmit = this.messageSubmit.bind(this);

    this.toggleCode = this.toggleCode.bind(this);

    this.editor = null;

    // perf testing:
    /* let closure = () => {
      let x = 0;
      for (var i = 0; i < 30; i++) {
        x++;
        props.api.chat.message(
          props.resource,
          `~${window.ship}`,
          Date.now(),
          {
            text: `${x}`
          }
        );
      }
      setTimeout(closure, 1000);
    };
    this.closure = closure.bind(this);*/

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

    console.log(editorMessage);

    if (editorMessage === '') {
      return;
    }

    if(state.code) {
      let post = props.api.createPost([
        {
          code: {
            expression: editorMessage,
            output: null
          }
        }
      ]);

      props.api.addPost(props.resource.ship, props.resource.name, post);
      this.editor.setValue('');
      return;
    }

    let message = this.getLetterType(editorMessage);
    let post = props.api.createPost([message]);
    props.api.addPost(props.resource.ship, props.resource.name, post);

    // perf:
    // setTimeout(this.closure, 2000);

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

  render() {
    const { props, state } = this;


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
        <div
          className="fl"
          style={{
            marginTop: 6,
            flexBasis: 24,
            height: 24
          }}
        >
        <Sigil
          ship={window.ship}
          size={24}
          color={`#000`}
          />
        </div>
        <div
          className="fr h-100 flex bg-gray0-d lh-copy pl2 w-100 items-center"
          style={{ flexGrow: 1, maxHeight: '224px', width: 'calc(100% - 72px)' }}
        >
          <CodeEditor
            options={options}
            editorDidMount={(editor) => {
            this.editor = editor;
            if (!/Android|webOS|iPhone|iPad|iPod|BlackBerry/i.test(
                navigator.userAgent
              )) {
              editor.focus();
              }
            }}
          />
        </div>
        <div className="ml2 mr2"
          style={{ height: '16px', width: '16px', flexBasis: 16, marginTop: 10 }}>
        </div>
        <div style={{ height: '16px', width: '16px', flexBasis: 16, marginTop: 10 }}>
          <img
            style={{ filter: state.code && 'invert(100%)', height: '100%', width: '100%' }}
            onClick={this.toggleCode}
            src="/~chat/img/CodeEval.png"
            className="contrast-10-d bg-white bg-none-d ba b--gray1-d br1"
          />
        </div>
      </div>
    );
  }
}
