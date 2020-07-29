import React, { Component } from 'react';
import { OverlaySigil } from './overlay-sigil';
import MessageContent from './message-content';
import { uxToHex, cite, writeText } from '../../../../lib/util';
import moment from 'moment';


export class Message extends Component {
  constructor() {
    super();
    this.state = {
      copied: false
    };
  }

  render() {
    const { props, state } = this;

    const pending = props.msg.pending ? ' o-40' : '';
    const containerClass =
      props.renderSigil ?
        `w-100 f7 pl3 pt4 pr3 cf flex lh-copy ` + pending :
        'w-100 pr3 cf hide-child flex' + pending;

    const timestamp =
      moment.unix(props.msg.when / 1000).format(
        props.renderSigil ? 'hh:mm a' : 'hh:mm'
      );

    return (
      <div
        ref={this.containerRef}
        className={containerClass}
        style={{
          minHeight: 'min-content'
        }}
      >
        {
          props.renderSigil ? (
            this.renderWithSigil(timestamp)
          ) : (
            this.renderWithoutSigil(timestamp)
          )
        }
      </div>
    );
  }

  renderWithSigil(timestamp) {
    const { props, state } = this;
    const paddingTop = props.paddingTop ? { 'paddingTop': '6px' } : '';
    const datestamp =
      '~' + moment.unix(props.msg.when / 1000).format('YYYY.M.D');

    const contact = props.msg.author in props.contacts
      ? props.contacts[props.msg.author] : false;
    let name = `~${props.msg.author}`;
    let color = '#000000';
    let sigilClass = 'mix-blend-diff';
    if (contact) {
      name = (contact.nickname.length > 0)
        ? contact.nickname : `~${props.msg.author}`;
      color = `#${uxToHex(contact.color)}`;
      sigilClass = '';
    }

    if (`~${props.msg.author}` === name) {
      name = cite(props.msg.author);
    }

    return (
      <div className="flex w-100">
        <OverlaySigil
          ship={props.msg.author}
          contact={contact}
          color={color}
          sigilClass={sigilClass}
          association={props.association}
          group={props.group}
          className="fl pr3 v-top bg-white bg-gray0-d"
        />
        <div className="fr clamp-message white-d"
             style={{ flexGrow: 1, marginTop: -8 }}>
          <div className="hide-child" style={paddingTop}>
            <p className={`v-mid f9 gray2 dib mr3 c-default`}>
              <span
                className={
                  'pointer ' +
                  (contact.nickname || state.copied ? null : 'mono')
                }
                onClick={() => {
                  writeText(props.msg.author);
                  this.setState({ copied: true });
                  setTimeout(() => {
                    this.setState({ copied: false });
                  }, 800);
                }}
                title={`~${props.msg.author}`}
              >
                {state.copied && 'Copied' || name}
              </span>
            </p>
            <p className={`v-mid mono f9 gray2 dib`}>{timestamp}</p>
            <p className={`v-mid mono f9 ml2 gray2 dib child dn-s`}>
              {datestamp}
            </p>
          </div>
          <MessageContent letter={props.msg.letter} />
        </div>
      </div>
    );
  }

  renderWithoutSigil(timestamp) {
    const { props } = this;

    return (
      <div className="flex w-100">
        <p className="child pt2 pl2 pr1 mono f9 gray2 dib">{timestamp}</p>
        <div className="fr f7 clamp-message white-d pr3 lh-copy"
             style={{ flexGrow: 1 }}>
        <MessageContent letter={props.msg.letter} />
        </div>
      </div>
    );
  }
}
