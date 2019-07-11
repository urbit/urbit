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
      message: ""
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
    this.setState({message: event.target.value});
  }

  messageSubmit() {
    const { props, state } = this;
    let message = {
      uid: uuid(),
      aut: window.ship,
      wen: Date.now(),
      aud: [props.station],
      sep: {
        lin: {
          msg: state.message,
          pat: false
        }
      }
    };

    props.api.hall(
      {
        convey: [message]
      }
    );

    this.setState({
      message: ""
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
      <div className="mt2 pa3 cf flex black bt">
        <div className="fl" style={{ flexBasis: 35, height: 40 }}>
          <Sigil ship={window.ship} size={32} />
        </div>
        <div className="fr h-100 flex" style={{ flexGrow: 1, height: 40 }}>
          <input className="ml2 bn"
            style={{ flexGrow: 1 }}
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
