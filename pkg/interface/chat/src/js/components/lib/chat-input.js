import React, { Component } from 'react';
import _ from 'lodash';
import moment from 'moment';
import Mousetrap from 'mousetrap';
import cn from 'classnames';

import { Sigil } from '/components/lib/icons/sigil';

import { uuid, uxToHex, hexToRgba } from '/lib/util';

const DEFAULT_INPUT_HEIGHT = 28;


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
      textareaHeight: DEFAULT_INPUT_HEIGHT,
      patpSuggestions: [],
      selectedSuggestion: null
    };

    this.textareaRef = React.createRef();

    this.messageSubmit = this.messageSubmit.bind(this);
    this.messageChange = this.messageChange.bind(this);

    this.onEnter = this.onEnter.bind(this);

    this.patpAutocomplete = this.patpAutocomplete.bind(this);
    this.nextAutocompleteSuggestion = this.nextAutocompleteSuggestion.bind(this);
    this.completePatp = this.completePatp.bind(this);

    this.clearSuggestions = this.clearSuggestions.bind(this);

    // Call once per frame @ 60hz
    this.textareaInput = _.debounce(this.textareaInput.bind(this), 16);

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

  componentDidMount() {
    this.bindShortcuts();
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
    this.setState({
      message: this.state.message.replace(
        /[a-zA-Z\-]*$/,
        suggestion
      ),
      patpSuggestions: []
    });
  }

  onEnter(e) {
    if (this.state.patpSuggestions.length !== 0) {
      this.completePatp(this.state.selectedSuggestion);
    } else {
      this.messageSubmit(e);
    }
  }

  bindShortcuts() {
    let mousetrap = Mousetrap(this.textareaRef.current);
    mousetrap.bind('enter', e => {
      e.preventDefault();
      e.stopPropagation();

      this.onEnter(e);
    });


    mousetrap.bind('tab', e => {
      e.preventDefault();
      e.stopPropagation();
      if(this.state.patpSuggestions.length === 0) {
        this.patpAutocomplete(this.state.message, true);
      } else {
        this.nextAutocompleteSuggestion(false);
      }
    });
    mousetrap.bind(['up', 'shift+tab'], e => {
      if(this.state.patpSuggestions.length !== 0) {
        e.preventDefault();
        e.stopPropagation();
        this.nextAutocompleteSuggestion(true)
      }

    });
    mousetrap.bind('down', e => {
      if(this.state.patpSuggestions.length !== 0) {
        e.preventDefault();
        e.stopPropagation();
        this.nextAutocompleteSuggestion(false)
      }
    });
    mousetrap.bind('esc', e => {
      if(this.state.patpSuggestions.length !== 0) {
        e.preventDefault();
        e.stopPropagation();
        this.clearSuggestions();
      }})
  }

  messageChange(event) {
    const message = event.target.value;
    this.setState({
      message
    });

    const { patpSuggestions } = this.state;
    if(patpSuggestions.length !== 0) {
      this.patpAutocomplete(message, false);
    }

  }

  textareaInput() {
    const newHeight = this.textareaRef.current.scrollHeight < DEFAULT_INPUT_HEIGHT * 8
      ? `${this.textareaRef.current.scrollHeight}px`
      : `${DEFAULT_INPUT_HEIGHT * 8}px`

    this.setState({
      textareaHeight: newHeight
    });
  }

  getLetterType(letter) {
    if (letter[0] === '#') {
      letter = letter.slice(1);
      // remove insignificant leading whitespace.
      // aces might be relevant to style.
      while (letter[0] === '\n') {
        letter = letter.slice(1);
      }

      return {
        code: {
          expression: letter,
          output: undefined
        }
      }
    } else if (letter[0] === '@') {
      letter = letter.slice(1);
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
    const { props, state } = this;

    if (state.message === '') {
      return;
    }

    let message = [];
    state.message.split(" ").map((each) => {
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

    // perf: setTimeout(this.closure, 2000);

    this.setState({
      message: '',
      textareaHeight: DEFAULT_INPUT_HEIGHT
    });
  }

  readOnlyRender() {
    const { props } = this;
    let color = !!props.ownerContact
      ? uxToHex(props.ownerContact.color) : '000000';

    let sigilClass = !!props.ownerContact
      ? "" : "mix-blend-diff";

    return (
      <div className="pa3 cf flex black bt b--gray4 o-50">
        <div className="fl" style={{
          marginTop: 4,
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
        <div className="fr h-100 flex" style={{ flexGrow: 1, height: 28, paddingTop: 6, resize: "none" }}>
          <p className="pl3">This chat is read only and you cannot post.</p>
        </div>
      </div>
    );
  }

  writeAccessRender() {
    const { props, state } = this;

    let color = !!props.ownerContact
      ? uxToHex(props.ownerContact.color) : '000000';

    let sigilClass = !!props.ownerContact
      ? "" : "mix-blend-diff";

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
            marginTop: 4,
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
        <div className="fr h-100 flex bg-gray0-d" style={{ flexGrow: 1 }}>
          <textarea
            className={"pl3 bn bg-gray0-d white-d"}
            style={{ flexGrow: 1, height: state.textareaHeight, paddingTop: 6, resize: "none" }}
            autoCapitalize="none"
            autoFocus={(
              /Android|webOS|iPhone|iPad|iPod|BlackBerry/i.test(
              navigator.userAgent
            )) ? false : true}
            ref={this.textareaRef}
            placeholder={props.placeholder}
            value={state.message}
            onChange={this.messageChange}
            onInput={this.textareaInput}
          />
        </div>
      </div>
    );
  }

  render() {
    const { props, state } = this;

    let writePermission = props.permissions[`/chat${props.station}/write`];
    if (writePermission) {
      if (writePermission.kind === 'black') {
        // black
        if (writePermission.who.has(window.ship)) {
          return this.readOnlyRender();
        } else {
          return this.writeAccessRender();
        }
      } else if (writePermission.kind === 'white') {
        // white
        if (writePermission.who.has(window.ship)) {
          return this.writeAccessRender();
        } else {
          return this.readOnlyRender();
        }
      }
    } else {
      return this.writeAccessRender();
    }
  }
}
