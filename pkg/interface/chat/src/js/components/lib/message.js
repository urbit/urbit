import React, { Component } from 'react';
import { Sigil } from '/components/lib/icons/sigil';
import classnames from 'classnames';
import moment from 'moment';
import _ from 'lodash';

export class Message extends Component {

  renderSpeech(speech) {
    if (_.has(speech, 'lin')) {
      return this.renderLin(speech.lin.msg, speech.lin.pat);
    } else if (_.has(speech, 'url')) {
      return this.renderUrl(speech.url);
    } else {
      return this.renderUnknown();
    }
  }

  renderUnknown() {
    return this.renderLin('<unknown message type>')
  }

  renderLin(content, action = false) {
    return (
      <p className={`body-regular-400 v-top ${action ? 'fs-italic' : ''}`}>
        {content}
      </p>
    );
  }

  renderUrl(url) {
    try {
      let urlObject = new URL(url);
      let imgMatch =
        /(jpg|img|png|gif|tiff|jpeg|JPG|IMG|PNG|TIFF|GIF|webp|WEBP|webm|WEBM)$/
        .exec(
           urlObject.pathname
         );
      if (imgMatch) {
        return this.renderImageUrl(url);
      } else {
        let localUrl = this.localizeUrl(url);
        return this.renderAnchor(localUrl, url);
      }
    } catch(e) {
      console.error('url render error', e);
      return this.renderAnchor(url);
    }
  }

  renderImageUrl(url) {
    return this.renderAnchor(url, (
      <img
        src={url}
        style={{
          width:"50%",
          maxWidth: '250px'
        }}
      ></img>
    ));
  }

  renderAnchor(href, content) {
    content = content || href;
    return (
      <a className="body-regular"
        href={href}
        target="_blank">{content}</a>
    );
  }

  renderContent() {
    const { props } = this;

    try {
      if (!_.has(props.msg, 'sep')) {
        return this.renderUnknown();
      }
      return this.renderSpeech(props.msg.sep);
    } catch (e) {
      console.error('speech rendering error', e);
      return this.renderUnknown();
    }
  }

  localizeUrl(url) {
    if (typeof url !== 'string') { throw 'Only transmogrify strings!'; }
    const ship = window.ship;
    if (url.indexOf('arvo://') === 0) {
      return `http://${ship}.arvo.network` + url.split('arvo://')[1];
    }
    return url;
  }

  render() {
    const { props } = this;
    let pending = !!props.msg.pending ? ' o-80' : '';
    let timestamp = moment.unix(props.msg.wen / 1000).format('hh:mm');
    let datestamp = moment.unix(props.msg.wen / 1000).format('LL');

    return (
      <div className={"w-100 pl3 pr3 pt2 pb2 cf flex" + pending}
        style={{
          minHeight: 'min-content'
        }}>
        <div className="fl mr2">
          <Sigil ship={props.msg.aut} size={36} />
        </div>
        <div className="fr" style={{ flexGrow: 1, marginTop: -8 }}>
          <div className="hide-child">
            <p className="v-top label-small-mono gray dib mr3">
              ~{props.msg.aut}
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
  }

}

