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
    } else if (_.has(speech, 'app')) {
      return this.renderSpeech(speech.app.sep);
    } else {
      return this.renderUnknown();
    }
  }

  renderUnknown() {
    return this.renderLin('<unknown message type>')
  }

  renderLin(content, action = false) {
    //TODO remove once arvo:// urls are supported in url speeches
    if (content.indexOf('arvo://') === 0) {
      return this.renderUrl(content);
    }
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

  renderAuthor() {
    const msg = this.props.msg;
    const ship = '~' + msg.aut;
    if (_.has(msg, 'sep.app.app')) {
      return `:${msg.sep.app.app} (${ship})`;
    } else {
      return ship;
    }
  }

  //NOTE see also lib/chat-input's globalizeUrl
  localizeUrl(url) {
    if (typeof url !== 'string') { throw 'Only localize strings!'; }
    const arvo = 'arvo://';
    if (url.indexOf(arvo) === 0) {
      // this application is being served by an urbit also, so /path will
      // point to the arvo url as hosted by this same urbit.
      let path = url.slice(arvo.length);
      // ensure single leading /
      if (path[0] !== '/') path = '/' + path;
      return path;
    } else {
      return url;
    }
  }

  render() {
    const { props } = this;
    let pending = !!props.msg.pending ? ' o-80' : '';
    let datestamp = moment.unix(props.msg.wen / 1000).format('LL');

    let paddingTop = props.paddingTop ? 'pt3' : '';
    let paddingBot = props.paddingBot ? 'pb2' : 'pb1';

    if (props.renderSigil) {
      let timestamp = moment.unix(props.msg.wen / 1000).format('hh:mm a');

      return (
        <div className={"w-100 pl3 pr3 cf flex " + paddingTop + " " + paddingBot + pending}
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
        <div className="fr" style={{ flexGrow: 1, marginTop: -8 }}>
          <div className="hide-child">
            <p className="v-top label-small-mono gray dib mr3">
              {this.renderAuthor()}
            </p>
            <p className="v-top label-small-mono gray dib">{timestamp}</p>
            <p className="v-top label-small-mono ml2 gray dib child">
              {datestamp}
            </p>
          </div>
        </div>
      )
    }
  }

}
