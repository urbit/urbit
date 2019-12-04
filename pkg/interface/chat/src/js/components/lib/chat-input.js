import React, { Component } from 'react';
import _ from 'lodash';
import moment from 'moment';
import Mousetrap from 'mousetrap';
import classnames from 'classnames';

import { Sigil } from '/components/lib/icons/sigil';

import { uuid } from '/lib/util';


export class ChatInput extends Component {

  constructor(props) {
    super(props);

    this.state = {
      message: '',
    };

    this.textareaRef = React.createRef();

    this.messageSubmit = this.messageSubmit.bind(this);
    this.messageChange = this.messageChange.bind(this);

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
  
  bindShortcuts() {
    Mousetrap(this.textareaRef.current).bind('enter', e => {
      e.preventDefault();
      e.stopPropagation();

      this.messageSubmit(e);
    });
  }

  messageChange(event) {
    this.setState({
      message: event.target.value
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
      const urlObject = new URL(string);
      //NOTE we check for a host to ensure a url is actually being posted
      //     to combat false positives for things like "marzod: ur cool".
      //     this does mean you can't send "mailto:e@ma.il" as %url message,
      //     but the desirability of that seems questionable anyway.
      return (urlObject.host !== '');
    } catch (e) {
      return false;
    }
  }

  messageSubmit() {
    const { props, state } = this;

    if (state.message === '') {
      return;
    }

    let letter = this.getLetterType(state.message);

    props.api.chat.message(
      props.station,
      `~${window.ship}`,
      Date.now(),
      letter
    );
    // perf: setTimeout(this.closure, 2000);

    this.setState({
      message: '',
    });
  }

  readOnlyRender() {
    return (
      <div className="pa3 cf flex black bt b--gray4 o-50">
        <div className="fl" style={{
          marginTop: 4,
          flexBasis: 24,
          height: 24
        }}>
          <Sigil ship={window.ship} size={24} color="#4330FC" />
        </div>
        <div className="fr h-100 flex" style={{ flexGrow: 1, height: 28, paddingTop: 6, resize: "none" }}>
          <p className="pl3">This chat is read only and you cannot post.</p>
        </div>
      </div>
    );
  }

  writeAccessRender() {
    const { props, state } = this;

    this.bindShortcuts();
    
    return (
      <div className="pa3 cf flex black bt b--gray4" style={{ flexGrow: 1 }}>
        <div
          className="fl"
          style={{
            marginTop: 4,
            flexBasis: 24,
            height: 24
          }}>
          <Sigil ship={window.ship} size={24} color="#4330FC" />
        </div>
        <div className="fr h-100 flex" style={{ flexGrow: 1 }}>
          <textarea
            className={"pl3 bn"}
            style={{ flexGrow: 1, height: 28, paddingTop: 6, resize: "none" }}
            autoCapitalize="none"
            autoFocus={(
              /Android|webOS|iPhone|iPad|iPod|BlackBerry/i.test(
              navigator.userAgent
            )) ? false : true}
            ref={this.textareaRef}
            placeholder={props.placeholder}
            value={state.message}
            onChange={this.messageChange}
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
