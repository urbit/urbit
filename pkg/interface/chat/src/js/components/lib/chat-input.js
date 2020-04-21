import React, { Component } from 'react';
import _ from 'lodash';
import moment from 'moment';
import Mousetrap from 'mousetrap';
import cn from 'classnames';
import { UnControlled as CodeEditor } from 'react-codemirror2'
import CodeMirror from 'codemirror';

import 'codemirror/mode/markdown/markdown';
import 'codemirror/addon/display/placeholder';

import { Sigil } from '/components/lib/icons/sigil';

import { uuid, uxToHex, hexToRgba } from '/lib/util';

const MARKDOWN_CONFIG = {
  name: "markdown",
  tokenTypeOverrides: {
    header: "presentation",
    quote: "presentation",
    list1: "presentation",
    list2: "presentation",
    list3: "presentation",
    hr: "presentation",
    image: "presentation",
    imageAltText: "presentation",
    imageMarker: "presentation",
    formatting: "presentation",
    linkInline: "presentation",
    linkEmail: "presentation",
    linkText: "presentation",
    linkHref: "presentation",
  }
}

// line height
const INPUT_LINE_HEIGHT = 28;

const INPUT_TOP_PADDING = 3;


function getAdvance(a, b) {
  let res = '';
  if(!a) {
    return b;
  }
  for (let i = 0; i < Math.min(a.length, b.length); i++) {
    if (a[i] !== b[i]) {
      return res;
    }
    res = res.concat(a[i]);
  }
  return res;
}

function ChatInputSuggestion({ ship, contacts, selected, onSelect }) {
  let contact = contacts[ship];
  let color = "#000000";
  let sigilClass = "v-mid mix-blend-diff"
  let nickname;
  let nameStyle = {};
  const isSelected = ship === selected;
  if (contact) {
    const hex = uxToHex(contact.color);
    color = `#${hex}`;
    nameStyle.color = hexToRgba(hex, .7);
    nameStyle.textShadow = '0px 0px 0px #000';
    nameStyle.filter = 'contrast(1.3) saturate(1.5)';
    sigilClass = "v-mid";
    nickname = contact.nickname;
  }

  return (
    <div
      onClick={() => onSelect(ship)}
      className={cn(
        'f8 pv1 ph3 pointer hover-bg-gray1-d hover-bg-gray4 relative flex items-center',
        {
          'white-d bg-gray0-d bg-white': !isSelected,
          'black-d bg-gray1-d bg-gray4': isSelected,
        }
      )}
      key={ship}
    >
      <Sigil
        ship={'~' + ship}
        size={24}
        color={color}
        classes={sigilClass}
      />
      { nickname && (
        <p style={nameStyle} className="dib ml4 b" >{nickname}</p>)
      }
      <div className="mono gray2 ml4">
        {'~' + ship}
      </div>
      <p className="nowrap ml4">
        {status}
      </p>
    </div>
  );

}

function ChatInputSuggestions({ suggestions, onSelect, selected, contacts }) {
  return (
    <div
      style={{
        bottom: '90%',
        left: '48px'
      }}
      className={
        'absolute black white-d bg-white bg-gray0-d ' +
        'w7 pv3 z-1 mt1 ba b--gray1-d b--gray4'
      }>
      {suggestions.map(ship =>
        (<ChatInputSuggestion
           onSelect={onSelect}
           key={ship}
           selected={selected}
           contacts={contacts}
           ship={ship} />)
      )}
    </div>
  );
}

export class ChatInput extends Component {
  constructor(props) {
    super(props);

    this.state = {
      message: '',
      patpSuggestions: [],
      selectedSuggestion: null
    };

    this.textareaRef = React.createRef();

    this.messageSubmit = this.messageSubmit.bind(this);
    this.messageChange = this.messageChange.bind(this);

    this.patpAutocomplete = this.patpAutocomplete.bind(this);
    this.nextAutocompleteSuggestion = this.nextAutocompleteSuggestion.bind(this);
    this.completePatp = this.completePatp.bind(this);

    this.clearSuggestions = this.clearSuggestions.bind(this);

    this.toggleCode = this.toggleCode.bind(this);

    this.editor = null;


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
    this.closure = closure.bind(this);*/

    moment.updateLocale('en', {
        relativeTime : {
            past: function(input) {
              return input === 'just now'
                ? input
                : input + ' ago'
            },
            s  : 'just now',
            future: "in %s",
            ss : '%d sec',
            m:  "a minute",
            mm: "%d min",
            h:  "an hr",
            hh: "%d hrs",
            d:  "a day",
            dd: "%d days",
            M:  "a month",
            MM: "%d months",
            y:  "a year",
            yy: "%d years"
        }
    });
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


  patpAutocomplete(message, fresh = false) {
    const match = /~([a-zA-Z\-]*)$/.exec(message);

    if (!match ) {
      this.setState({ patpSuggestions: [] })
      return;
    }


    const needle = match[1].toLowerCase();

    const matchString = hay => {
      hay = hay.toLowerCase();

      return hay.startsWith(needle)
        || _.some(_.words(hay), s => s.startsWith(needle));
    };


    const contacts = _.chain(this.props.contacts)
      .defaultTo({})
      .map((details, ship) => ({...details, ship }))
      .filter(({ nickname, ship }) => matchString(nickname) || matchString(ship))
      .map('ship')
      .value()

    const suggestions = _.chain(this.props.envelopes)
      .defaultTo([])
      .map("author")
      .uniq()
      .reverse()
      .filter(matchString)
      .union(contacts)
      .filter(s => s.length < 28) // exclude comets
      .take(5)
      .value();

    let newState = {
      patpSuggestions: suggestions,
      selectedSuggestion: suggestions[0]
    };

    this.setState(newState);
  }

  clearSuggestions() {
    this.setState({
      patpSuggestions: []
    })
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
      patpSuggestions: []
    });
  }

  messageChange(editor, data, value) {

    const { patpSuggestions } = this.state;
    if(patpSuggestions.length !== 0) {
      this.patpAutocomplete(value, false);
    }

  }


  getLetterType(letter) {
    if (letter.startsWith('/me')) {
      letter = letter.slice(3);
      // remove insignificant leading whitespace.
      // aces might be relevant to style.
      while (letter[0] === '\n') {
        letter = letter.slice(1);
      }

      return {
        me: letter
      }
    } else if (this.isUrl(letter)) {
       return {
        url: letter
      }
    } else {
      return {
        text: letter
      }
    }
  }

  isUrl(string) {
    try {
      let websiteTest = new RegExp(''
      + /((\w+:\/\/)[-a-zA-Z0-9:@;?&=\/%\+\.\*!'\(\),\$_\{\}\^~\[\]`#|]+)/.source
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
    let message = [];
    editorMessage.split(" ").map((each) => {
      if (this.isUrl(each)) {
        if (message.length > 0) {
          message = message.join(" ");
          message = this.getLetterType(message);
          props.api.chat.message(
            props.station,
            `~${window.ship}`,
            Date.now(),
            message
          );
          message = [];
        }
        let URL = this.getLetterType(each);
        props.api.chat.message(
          props.station,
          `~${window.ship}`,
          Date.now(),
          URL
        );
      }
      else {
        return message.push(each);
      }
    })


    if (message.length > 0) {
      message = message.join(" ");
      message = this.getLetterType(message);
      props.api.chat.message(
        props.station,
        `~${window.ship}`,
        Date.now(),
        message
      );
      message = [];
    }

    // perf:
    //setTimeout(this.closure, 2000);

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
      this.editor.setOption('placeholder', "Code...");
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

    let color = !!props.ownerContact
      ? uxToHex(props.ownerContact.color) : '000000';

    let sigilClass = !!props.ownerContact
      ? "" : "mix-blend-diff";

    const completeActive = this.state.patpSuggestions.length !== 0;

    const codeTheme = state.code ? ' code' : '';

    const options = {
      mode: MARKDOWN_CONFIG,
      theme: 'tlon' + codeTheme,
      lineNumbers: false,
      lineWrapping: true,
      scrollbarStyle: 'native',
      cursorHeight: 0.85,
      placeholder: state.code ? "Code..." : props.placeholder,
      extraKeys: {
        Tab: (cm) =>
          completeActive
            ? this.nextAutocompleteSuggestion()
            : this.patpAutocomplete(cm.getValue(), true),
        'Shift-Tab': (cm) =>
          completeActive
            ? this.nextAutocompleteSuggestion(true)
            : CodeMirror.Pass,
        'Up': (cm) =>
          completeActive
            ? this.nextAutocompleteSuggestion(true)
            : CodeMirror.Pass,
        'Escape': (cm) =>
          completeActive
            ? this.clearSuggestions(true)
            : CodeMirror.Pass,
        'Down': (cm) =>
          completeActive
            ?  this.nextAutocompleteSuggestion()
            :  CodeMirror.Pass,
        'Enter': (cm) =>
          completeActive
            ? this.completePatp(state.selectedSuggestion)
            : this.messageSubmit(),
        'Shift-3': (cm) =>
          cm.getValue().length === 0
            ? this.toggleCode()
            : CodeMirror.Pass
      }
    };


    return (
      <div className="pa3 cf flex black white-d bt b--gray4 b--gray1-d bg-white bg-gray0-d relative"
      style={{ flexGrow: 1 }}>
        {state.patpSuggestions.length !== 0 && (
          <ChatInputSuggestions
            onSelect={this.completePatp}
            suggestions={state.patpSuggestions}
            selected={state.selectedSuggestion}
            contacts={props.contacts}
          />
        )}

        <div
          className="fl"
          style={{
            marginTop: 6,
            flexBasis: 24,
            height: 24
          }}>
          <Sigil
            ship={window.ship}
            size={24}
            color={`#${color}`}
            classes={sigilClass}
            />
        </div>
        <div
          className="fr h-100 flex bg-gray0-d lh-copy pl2 w-100 items-center"
          style={{ flexGrow: 1, maxHeight: '224px', width: 'calc(100% - 48px)' }}>
          <CodeEditor
            options={options}
            editorDidMount={editor => { this.editor = editor; }}
            onChange={(e, d, v) => this.messageChange(e, d, v)}
          />
        </div>
        <div style={{ height: '24px', width: '24px', flexBasis: 24, marginTop: 6 }}>
          <img
            style={{ filter: state.code && 'invert(100%)', height: '100%', width: '100%' }}
            onClick={this.toggleCode}
            src="/~chat/img/CodeEval.png"
            className="contrast-10-d bg-white bg-none-d"
          />

        </div>

      </div>
    );
  }
}
