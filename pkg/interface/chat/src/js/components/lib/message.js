import React, { Component } from 'react';
import { Sigil } from '/components/lib/icons/sigil';
import classnames from 'classnames';
import { Route, Link } from 'react-router-dom'
import urbitOb from 'urbit-ob';
import moment from 'moment';
import _ from 'lodash';


export class Message extends Component {

  renderContent() {
    const { props } = this;
    let letter = props.msg.letter;

    if ('code' in letter) {
      let outputElement = 
        (!!letter.code.output && 
         letter.code.output.length && letter.code.output.length > 0) ?
        (
          <pre className="clamp-attachment pa1 mt0 mb0">
            {letter.code.output[0].join('\n')}
          </pre>
        ) : null;
      return (
        <span>
          <pre className="clamp-attachment pa1 mt0 mb0 bg-light-gray">
            {letter.code.expression}
          </pre>
          {outputElement}
        </span>
      );
    } else if ('url' in letter) {
      let imgMatch =
        /(jpg|img|png|gif|tiff|jpeg|JPG|IMG|PNG|TIFF|GIF|webp|WEBP|webm|WEBM)$/
        .exec(letter.url);
      let contents = letter.url;
      if (imgMatch) {
        contents = (
          <img
            src={letter.url}
            style={{
              width: "50%",
              maxWidth: "250px"
            }}
          ></img>
        );
      }
      return (
        <a className="f7 lh-copy v-top bb b--black word-break-all"
          href={letter.url}
          target="_blank"
          rel="noopener noreferrer">
          {contents}
        </a>
      );
    } else if ('me' in letter) {
      return (
        <p className='f7 i lh-copy v-top'>
          {letter.me}
        </p>
      );
    } else {
      let chatroom = letter.text.match(
        /(~[a-z]{3,6})(-[a-z]{6})?([/])(([a-z])+([/-])?)+/
        );
      if ((chatroom !== null) // matched possible chatroom
        && (chatroom[1].length > 2) // possible ship?
        && (urbitOb.isValidPatp(chatroom[1]) // valid patp?
        && (chatroom[0] === letter.text))) { // entire message is room name?
          return (
            <Link
            className="bb b--black f7 mono lh-copy v-top" 
            to={"/~chat/join/" + chatroom.input}>
              {letter.text}
            </Link>
          );
        }
      else {
        return (
          <p className='f7 lh-copy v-top'>
            {letter.text}
          </p>
        );
      }
  }
  }

  render() {
    const { props } = this;
    let pending = !!props.msg.pending ? ' o-40' : '';
    let datestamp = "~" + moment.unix(props.msg.when / 1000).format('YYYY.MM.D');

    let paddingTop = props.paddingTop ? {'paddingTop': '6px'} : '';

    if (props.renderSigil) {
      let timestamp = moment.unix(props.msg.when / 1000).format('hh:mm a');

      return (
        <div
          className={
            "w-100 f8 pl3 pt4 pr3 cf flex lh-copy " + " " + pending
          }
          style={{
            minHeight: "min-content"
          }}>
          <div className="fl mr3 v-top">
            <Sigil
              ship={props.msg.author}
              size={24}
              color={((props.msg.author === window.ship) 
              || (props.msg.author.substr(1) === window.ship))
              ? "#4330FC" 
              : "#000000"}
            />
          </div>
          <div
            className="fr clamp-message"
            style={{ flexGrow: 1, marginTop: -8 }}>
            <div className="hide-child" style={paddingTop}>
              <p className="v-mid mono f9 gray dib mr3">
                {props.msg.author.slice(0, 1) === "~" ? "" : "~"}
                {props.msg.author}
              </p>
              <p className="v-mid mono f9 gray dib">{timestamp}</p>
              <p className="v-mid mono f9 ml2 gray dib child dn-s">{datestamp}</p>
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
          <p className="child pt2 pl2 pr1 mono f9 gray dib">{timestamp}</p>
          <div className="fr f7 clamp-message" style={{ flexGrow: 1 }}>
            {this.renderContent()}
          </div>
        </div>
      );
    }
  }
}
