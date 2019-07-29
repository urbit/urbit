import React, { Component } from 'react';
import _ from 'lodash';
import moment from 'moment';
import Mousetrap from 'mousetrap';
import classnames from 'classnames';

import { Sigil } from '/components/lib/icons/sigil';
import { IconSend } from '/components/lib/icons/icon-send';

import { uuid } from '/lib/util';


export class ChatInput extends Component {

  constructor(props) {
    super(props);

    /*let closure = () => {
      let aud, sep;
      let wen = Date.now();
      let aut = window.ship;

      let config = props.configs[props.station];

      aud = [props.station];
      sep = {
        lin: {
          msg: Date.now().toString(),
          pat: false
        }
      }

      let uid;
      let message;

      for (var i = 0; i < 10; i++) {
        uid = uuid();
        message = {
          uid,
          aut,
          wen,
          aud,
          sep,
        };

        props.api.hall({
          convey: [message]
        });
      }

      setTimeout(closure, 1000);
    };

    setTimeout(closure, 2000);*/

    this.state = {
      message: '',
      messageType: 'lin',
      clipboard: null
    };

    this.textareaRef = React.createRef();

    this.messageSubmit = this.messageSubmit.bind(this);
    this.messageChange = this.messageChange.bind(this);

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

  bindShortcuts() {
    Mousetrap(this.textareaRef.current).bind('enter', e => {
      e.preventDefault();
      e.stopPropagation();

      this.messageSubmit(e);
    });
  }

  messageChange(event) {
    const input = event.target.value;
    const previous = this.state.message;
    //NOTE dumb hack to work around paste event flow oddities
    const pasted = (previous.length === 0 && input.length > 1);
    if (input !== this.state.clipboard) {
      this.setState({
        message: input,
        messageType: this.getSpeechType(input),
        clipboard: (pasted ? input : null)
      });
    }
  }

  getSpeechType(input) {
    if (input.indexOf('\n') >= 0) {
      return 'fat';
    } else if (input[0] === '@') {
      return 'lin@';
    } else if (this.isUrl(input)) {
      return 'url';
    } else {
      return 'lin';
    }
  }

  getSpeechStyle(type, clipboard) {
    switch (type) {
      case 'lin@':
        return 'fs-italic';
      case 'url':
        return 'td-underline';
      case 'fat':
        if (clipboard) return 'code';
      default:
        return '';
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

  // turns select urls into arvo:// urls
  //
  //   we detect app names from the url. if the app is known to handle requests
  //   for remote data (instead of serving only from the host) we transfor the
  //   url into a generic arvo:// one.
  //   the app name format is pretty distinct and rare to find in the non-urbit
  //   wild, but this could still result in false positives for older-school
  //   websites serving pages under /~user paths.
  //   we could match only on ship.arvo.network, but that would exclude those
  //   running on localhost or under a custom domain.
  //
  //
  globalizeUrl(url) {
    const urlObject = new URL(url);
    const app = urlObject.pathname.split('/')[1];
    if (app === '~chat' ||
        app === '~publish') {
      //TODO send proper url speeches once hall starts using a url type that
      //     supports non-http protocols.
      return { lin: {
        msg: 'arvo://' + url.slice(urlObject.origin.length),
        pat: false
      } };
    } else {
      return {url};
    }
  }

  speechFromInput(content, type, clipboard) {
    switch (type) {
      case 'lin':
        return { lin: {
          msg: content,
          pat: false
        } };
      //
      case 'lin@':
        return { lin: {
          msg: content.slice(1),
          pat: true
        } };
      //
      case 'url':
        return this.globalizeUrl(content);
      //
      case 'fat':
        // clipboard contents
        if (clipboard !== null) {
          return { fat: {
            sep: { lin: { msg: '', pat: false } },
            tac: { name: {
              nom: 'clipboard',
              tac: { text: content }
            } }
          } };
        // long-form message
        } else {
          const lines = content.split('\n');
          return { fat: {
            sep: { lin: {
              msg: lines[0],
              pat: false
            } },
            tac: { name: {
              nom: 'long-form',
              tac: { tank: lines.slice(1).map(l => { return {leaf: l }; }) }
            } },
          } };
        }
      //
      default:
        throw new Error('Unimplemented speech type', type);
    }
  }

  messageSubmit() {
    const { props, state } = this;

    if (state.message === '') {
      return;
    }

    let message = {
      uid: uuid(),
      aut: window.ship,
      wen: Date.now(),
      aud: [props.station],
      sep: this.speechFromInput(
        state.message,
        state.messageType,
        state.clipboard
      )
    };

    props.api.hall(
      {
        convey: [message]
      }
    );

    this.setState({
      message: '',
      messageType: 'lin'
    });
  }

  readOnlyRender() {
    return (
      <div className="mt2 pa3 cf flex black bt o-50">
        <div className="fl" style={{ flexBasis: 35, height: 40 }}>
          <Sigil ship={window.ship} size={32} />
        </div>
        <div className="fr h-100 flex pa2" style={{ flexGrow: 1, height: 40 }}>
          <p>This chat is read only and you cannot post.</p>
        </div>
      </div>
    );
  }

  render() {
    const { props, state } = this;

    if (props.security && props.security.sec !== 'channel' &&
      !props.security.sis.includes(window.ship)) {
      return this.readOnlyRender();
    }

    return (
      <div className="pa3 cf flex black bt b--black-30" style={{ flexGrow: 1 }}>
        <div className="fl" style={{
          marginTop: 4,
          flexBasis: 32,
          height: 36
        }}>
          <Sigil ship={window.ship} size={32} />
        </div>
        <div className="fr h-100 flex" style={{ flexGrow: 1 }}>
          <textarea
            className={'ml2 mt2 mr2 bn ' +
              this.getSpeechStyle(state.messageType, state.clipboard)
            }
            style={{ flexGrow: 1, resize: 'none' }}
            ref={this.textareaRef}
            placeholder={props.placeholder}
            value={state.message}
            onChange={this.messageChange}
            autoFocus={true}
          />
          <div className="pointer" onClick={this.messageSubmit}>
            <IconSend />
          </div>
        </div>
      </div>
    );
  }
}
