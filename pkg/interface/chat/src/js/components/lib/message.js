import React, { Component } from 'react';
import { Sigil } from '/components/lib/icons/sigil';
import { ProfileOverlay } from '/components/lib/profile-overlay';
import { OverlaySigil } from '/components/lib/overlay-sigil';
import classnames from 'classnames';
import { Route, Link } from 'react-router-dom'
import { uxToHex, cite, writeText } from '/lib/util';
import urbitOb from 'urbit-ob';
import moment from 'moment';
import _ from 'lodash';
import ReactMarkdown from 'react-markdown';
import RemarkDisableTokenizers from 'remark-disable-tokenizers';

const DISABLED_BLOCK_TOKENS = [
  'indentedCode',
  'blockquote',
  'atxHeading',
  'thematicBreak',
  'list',
  'setextHeading',
  'html',
  'definition',
  'table',
];

const DISABLED_INLINE_TOKENS = [
  'autoLink',
  'url',
  'email',
  'link',
  'reference'
];

const MessageMarkdown = React.memo(
  props => (<ReactMarkdown
              {...props}
              plugins={[[RemarkDisableTokenizers, { block: DISABLED_BLOCK_TOKENS, inline: DISABLED_INLINE_TOKENS }]]}
            />));

export class Message extends Component {
  constructor() {
    super();
    this.state = {
      unfold: false,
      copied: false,
    };
    this.unFoldEmbed = this.unFoldEmbed.bind(this);
  }

  unFoldEmbed(id) {
    let unfoldState = this.state.unfold;
    unfoldState = !unfoldState;
    this.setState({unfold: unfoldState});
    let iframe = this.refs.iframe;
    iframe.setAttribute('src', iframe.getAttribute('data-src'));
  }


  renderContent() {
    const { props } = this;
    let letter = props.msg.letter;

    if ('code' in letter) {
      let outputElement =
        (!!letter.code.output &&
         letter.code.output.length && letter.code.output.length > 0) ?
        (
          <pre className="f7 clamp-attachment pa1 mt0 mb0">
            {letter.code.output[0].join('\n')}
          </pre>
        ) : null;
      return (
        <span>
          <pre className="f7 clamp-attachment pa1 mt0 mb0 bg-light-gray">
            {letter.code.expression}
          </pre>
          {outputElement}
        </span>
      );
    } else if ('url' in letter) {
      let imgMatch =
        /(jpg|img|png|gif|tiff|jpeg|JPG|IMG|PNG|TIFF|GIF|webp|WEBP|webm|WEBM)$/
        .exec(letter.url);
      let youTubeRegex = new RegExp(''
      + /(?:https?:\/\/(?:[a-z]+.)?)/.source // protocol
      + /(?:youtu\.?be(?:\.com)?\/)(?:embed\/)?/.source // short and long-links
      + /(?:(?:(?:(?:watch\?)?(?:time_continue=(?:[0-9]+))?.+v=)?([a-zA-Z0-9_-]+))(?:\?t\=(?:[0-9a-zA-Z]+))?)/.source // id
      )
      let ytMatch =
      youTubeRegex.exec(letter.url);
      let contents = letter.url;
      if (imgMatch) {
        contents = (
          <img
            className="o-80-d"
            src={letter.url}
            style={{
              width: "50%",
              maxWidth: "250px"
            }}
          ></img>
        );
        return (
          <a className="f7 lh-copy v-top word-break-all"
            href={letter.url}
            target="_blank"
            rel="noopener noreferrer">
            {contents}
          </a>
        );
      } else if (ytMatch) {
        contents = (
          <div className={'embed-container mb2 w-100 w-75-l w-50-xl ' +
          ((this.state.unfold === true)
            ? "db" : "dn")}>
          <iframe
            ref="iframe"
            width="560"
            height="315"
            data-src={`https://www.youtube.com/embed/${ytMatch[1]}`}
            frameBorder="0" allow="picture-in-picture, fullscreen">
          </iframe>
          </div>
        )
        return (
          <div>
          <a href={letter.url}
          className="f7 lh-copy v-top bb b--white-d word-break-all"
          href={letter.url}
          target="_blank"
          rel="noopener noreferrer">
            {letter.url}
        </a>
        <a className="ml2 f7 pointer lh-copy v-top"
        onClick={e => this.unFoldEmbed()}>
          [embed]
          </a>
        {contents}
        </div>
        )
      } else {
        return (
          <a className="f7 lh-copy v-top bb b--white-d b--black word-break-all"
            href={letter.url}
            target="_blank"
            rel="noopener noreferrer">
            {contents}
          </a>
        );
      }
    } else if ('me' in letter) {
      return (
        <p className='f7 i lh-copy v-top'>
          {letter.me}
        </p>
      );
    } else {
        return (
          <section>
            <MessageMarkdown
              source={letter.text}
            />
          </section>
        );
    }
  }

  render() {
    const { props, state } = this;
    let pending = !!props.msg.pending ? ' o-40' : '';
    let datestamp = "~" + moment.unix(props.msg.when / 1000).format('YYYY.M.D');

    let paddingTop = props.paddingTop ? {'paddingTop': '6px'} : '';

    if (props.renderSigil) {
      let timestamp = moment.unix(props.msg.when / 1000).format('hh:mm a');

      let contact = !!(props.msg.author in props.contacts)
        ? props.contacts[props.msg.author] : false;
      let name = `~${props.msg.author}`;
      let color = "#000000";
      let sigilClass = "mix-blend-diff"
      if (contact) {
        name = (contact.nickname.length > 0)
          ? contact.nickname : `~${props.msg.author}`;
        color = `#${uxToHex(contact.color)}`;
        sigilClass = "";
      }

      if (`~${props.msg.author}` === name) {
        name = cite(props.msg.author);
      }

      return (
        <div
          ref={this.containerRef}
          className={
            "w-100 f7 pl3 pt4 pr3 cf flex lh-copy " + " " + pending
          }
          style={{
            minHeight: "min-content"
          }}>
         <OverlaySigil
           ship={props.msg.author}
           contact={contact}
           color={color}
           sigilClass={sigilClass}
           group={props.group}
           className="fl pr3 v-top bg-white bg-gray0-d" />
          <div
            className="fr clamp-message white-d"
            style={{ flexGrow: 1, marginTop: -8 }}>
            <div className="hide-child" style={paddingTop}>
              <p className="v-mid f9 gray2 dib mr3 c-default">
                <span
                  className={"pointer " + (contact.nickname || state.copied ? null : "mono")}
                  onClick={() => {
                    writeText(props.msg.author);
                    this.setState({ copied: true })
                    setTimeout(() => {
                      this.setState({ copied: false });
                    }, 800)
                  }}
                  title={`~${props.msg.author}`}>
                  {state.copied && 'Copied' || name}
                </span>
              </p>
              <p className="v-mid mono f9 gray2 dib">{timestamp}</p>
              <p className="v-mid mono f9 ml2 gray2 dib child dn-s">{datestamp}</p>
            </div>
            {this.renderContent()}
          </div>
        </div>
      );
    } else {
      let timestamp = moment.unix(props.msg.when / 1000).format('hh:mm');

      return (
        <div
          className={"w-100 pr3 cf hide-child flex" + pending}
          style={{
            minHeight: "min-content"
          }}>
          <p className="child pt2 pl2 pr1 mono f9 gray2 dib">{timestamp}</p>
          <div className="fr f7 clamp-message white-d pr3 lh-copy" style={{ flexGrow: 1 }}>
           {this.renderContent()}
          </div>
        </div>
      );
    }
  }
}
