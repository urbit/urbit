import React, { Component } from 'react';
import _ from 'lodash';
import moment from 'moment';
import Mousetrap from 'mousetrap';

import { Sigil } from '/components/lib/icons/sigil';
import { ShipSearch } from '/components/lib/ship-search';

import { uuid, uxToHex, hexToRgba } from '/lib/util';


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


export class ChatInput extends Component {
  constructor(props) {
    super(props);

    this.state = {
      message: '',
      textareaHeight: INPUT_LINE_HEIGHT + INPUT_TOP_PADDING + 1,
      patpSearch: ''
    };

    this.textareaRef = React.createRef();

    this.messageSubmit = this.messageSubmit.bind(this);
    this.messageChange = this.messageChange.bind(this);


    this.patpAutocomplete = this.patpAutocomplete.bind(this);
    this.completePatp = this.completePatp.bind(this);


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


  patpAutocomplete(message, fresh = false) {
    const match = /~([a-zA-Z\-]*)$/.exec(message);

    if (!match ) {
      this.bindShortcuts();
      this.setState({ patpSearch: '' })
      return;
    }
    this.unbindShortcuts();
    this.setState({ patpSearch: match[1].toLowerCase() });

  }

  clearSearch() {
    this.setState({
      patpSearch: ''
    })
  }

  completePatp(suggestion) {
    this.bindShortcuts();
    this.setState({
      message: this.state.message.replace(
        /[a-zA-Z\-]*$/,
        suggestion
      ),
      patpSearch: ''
    });
  }


  bindShortcuts() {
    if(!this.mousetrap) {
      this.mousetrap = new Mousetrap(this.textareaRef.current);
    }
    this.mousetrap.bind('enter', e => {
      e.preventDefault();

      if(this.state.patpSearch.length === 0) {
        this.messageSubmit();
      }
    });


    this.mousetrap.bind('tab', e => {
      e.preventDefault();
      e.stopPropagation();
      if(this.state.patpSearch.length === 0) {
        this.patpAutocomplete(this.state.message, true);
      }
    });
  }

  unbindShortcuts() {
    if(!this.mousetrap) {
      return;
    }
    this.mousetrap.unbind('enter')
    this.mousetrap.unbind('tab')
  }


  messageChange(event) {
    const message = event.target.value;
    this.setState({
      message
    });

    const { patpSearch } = this.state;
    if(patpSearch.length !== 0) {
      this.patpAutocomplete(message, false);
    }

  }

  textareaInput() {
    const maxHeight = INPUT_LINE_HEIGHT * 8 + INPUT_TOP_PADDING;
    const newHeight = `${Math.min(maxHeight, this.textareaRef.current.scrollHeight)}px`;

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

    // perf: 
    //setTimeout(this.closure, 2000);

    this.setState({
      message: '',
      textareaHeight: INPUT_LINE_HEIGHT + INPUT_TOP_PADDING + 1
    });
  }

  render() {
    const { props, state } = this;

    let color = !!props.ownerContact
      ? uxToHex(props.ownerContact.color) : '000000';

    let sigilClass = !!props.ownerContact
      ? "" : "mix-blend-diff";

    const candidates = _.chain(this.props.envelopes)
      .defaultTo([])
      .map("author")
      .uniq()
      .reverse()
      .value();

    return (
      <div className="pa3 cf flex black white-d bt b--gray4 b--gray1-d bg-white bg-gray0-d relative"
      style={{ flexGrow: 1 }}>
        <ShipSearch
          popover
          onSelect={this.completePatp}
          contacts={props.contacts}
          candidates={candidates}
          searchTerm={this.state.patpSearch}
          inputRef={this.textareaRef.current}
        />

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
            className={"pl3 bn bg-gray0-d white-d lh-copy"}
            style={{ flexGrow: 1, height: state.textareaHeight, paddingTop: INPUT_TOP_PADDING, resize: "none" }}
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
}
