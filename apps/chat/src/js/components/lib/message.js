import React, { Component } from 'react';
import { getMessageContent } from '/lib/util';
import { Sigil } from '/components/lib/icons/sigil';
import classnames from 'classnames';
import moment from 'moment';

export class Message extends Component {
  renderContent(type) {
    if (type === "url") {
        if (/(jpg|img|png|gif|tiff|jpeg|JPG|IMG|PNG|TIFF)$/.exec(this.props.details.content)) {
          return (
            <img src={this.props.details.content} style={{width:"30%"}}></img>
          )
        } else {
          return (
            <a className="body-regular"
              href={this.props.details.content}
              target="_blank">{this.props.details.content}</a>
          )
        }
    } else if (type === "exp") {
      return (
        <div>
          <div className="label-small-mono">{this.props.details.content}</div>
          <pre className="label-small-mono mt-0">{this.props.details.res}</pre>
        </div>
      )
    } else if (['new item', 'edited item'].includes(type)) {
      return <span className="text-body" dangerouslySetInnerHTML={{__html: this.props.details.snip}}></span>
    } else if (type === "lin") {
      return (
        <p className="body-regular-400 v-top">{this.props.details.content}</p>
      );
    } else {
      return <span className="label-small-mono">{'<unknown message type>'}</span>;
    }
  }

  render() {
    let pending = !!this.props.msg.pending ? ' o-80' : '';

    let timestamp = moment.unix(this.props.msg.wen / 1000).format('hh:mm');
    window.timestamp = timestamp;
    window.wen = this.props.msg.wen;
    window.moment = moment;
    
    return (
      <div className={"w-100 pl3 pr3 pt2 pb2 mb2 cf flex" + pending}>
        <div className="fl mr2">
          <Sigil ship={this.props.msg.aut} size={32} />
        </div>
        <div className="fr" style={{ flexGrow: 1, marginTop: -4 }}>
          <div>
            <p className="v-top label-small-mono gray dib mr3">~{this.props.msg.aut}</p>
            <p className="v-top label-small-mono gray dib">{timestamp}</p>
          </div>
          {this.renderContent(this.props.details.type)}
        </div>
      </div>
    );
  }
}

