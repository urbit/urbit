import React, { Component } from 'react';
import { Sigil } from '/components/lib/icons/sigil';
import classnames from 'classnames';
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
        <a className="body-regular-400 v-top"
          href={letter.url}
          target="_blank"
          rel="noopener noreferrer">
          {contents}
        </a>
      );
    } else if ('me' in letter) {
      return (
        <p className='body-regular-400 v-top'>
          {letter.me}
        </p>
      );
    } else {
      return (
        <p className='body-regular-400 v-top'>
          {letter.text}
        </p>
      );
    }
  }

  render() {
    const { props } = this;
    let pending = !!props.msg.pending ? ' o-40' : '';
    let datestamp = moment.unix(props.msg.when / 1000).format('LL');

    let paddingTop = props.paddingTop ? 'pt3' : '';
    let paddingBot = props.paddingBot ? 'pb2' : 'pb1';

    if (props.renderSigil) {
      let timestamp = moment.unix(props.msg.when / 1000).format('hh:mm a');

      return (
        <div className={"w-100 pl3 pr3 cf flex " + paddingTop + " " + paddingBot + pending}
             style={{
               minHeight: 'min-content'
             }}>
          <div className="fl mr2">
            <Sigil ship={props.msg.author} size={36} />
          </div>
          <div className="fr clamp-message" style={{ flexGrow: 1, marginTop: -8 }}>
            <div className="hide-child">
              <p className="v-top label-small-mono gray dib mr3">
                {props.msg.author}
              </p>
              <p className="v-top label-small-mono gray dib">{timestamp}</p>
              <p className="v-top label-small-mono ml2 gray dib child">
                {datestamp}
              </p>
            </div>
            {this.renderContent()}
          </div>
        </div>
      );
    } else {
      let timestamp = moment.unix(props.msg.when / 1000).format('hh:mm');

      return (
        <div className={"w-100 pr3 pb1 cf hide-child flex" + pending}
             style={{
               minHeight: 'min-content'
             }}>
          <p className="child pl3 pr2 label-small-mono gray dib">{timestamp}</p>
          <div className="fr clamp-message" style={{ flexGrow: 1 }}>
            {this.renderContent()}
          </div>
        </div>
      )
    }
  }
}
