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

    this.state = {
      message: '',
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
    this.setState({
      message: event.target.value
    });
  }

  messageSubmit() {
    const { props, state } = this;

    if (state.message === '') {
      return;
    }
    console.log(props.owner, window.ship);
    props.api.inbox.message(
      props.owner === `${window.ship}`,
      props.station,
      `~${window.ship}`,
      Date.now(),
      state.message
    );

    this.setState({
      message: '',
    });
  }

  readOnlyRender() {
    return (
      <div className="mt2 pa3 cf flex black bt o-50">
        <div className="fl" style={{
          marginTop: 4,
          flexBasis: 32,
          height: 36
        }}>
          <Sigil ship={window.ship} size={32} />
        </div>
        <div className="fr h-100 flex pa2" style={{ flexGrow: 1, height: 40 }}>
          <p style={{paddingTop: 3}}>This chat is read only and you cannot post.</p>
        </div>
      </div>
    );
  }

  render() {
    const { props, state } = this;

    /*if (props.security && props.security.sec !== 'channel' &&
      !props.security.sis.includes(window.ship)) {
      return this.readOnlyRender();
    }*/

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
            className={'ml2 mt2 mr2 bn'}
            style={{ flexGrow: 1, height: 40, paddingTop: 3, resize: 'none' }}
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
