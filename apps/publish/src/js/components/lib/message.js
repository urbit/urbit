import React, { Component } from 'react';
import { isDMStation, getMessageContent } from '/lib/util';
import { Sigil } from '/components/lib/icons/sigil';
import classnames from 'classnames';
import moment from 'moment';

export class Message extends Component {
  buildPostTitle(messageDetails) {
    if (messageDetails.postUrl) {
      return (
        <a className="pr-12 text-600 underline"
          href={messageDetails.postUrl}>
          {messageDetails.postTitle}
        </a>
      )
    } else {
      return null;
    }
  }

  renderContent(type) {
      if (type === "text") {
      return this.buildPostTitle(this.props.details);
    } else if (type === "url") {
        if (/(jpg|img|png|gif|tiff|jpeg|JPG|IMG|PNG|TIFF)$/.exec(this.props.details.content)) {
          return (
            <img src={this.props.details.content} style={{width:"30%"}}></img>
          )
        } else {
          return (
            <a href={this.props.details.content} target="_blank">{this.props.details.content}</a>
          )
        }
    } else if (type === "exp") {
      return (
        <div className="text-body">
          <div className="text-mono">{this.props.details.content}</div>
          <pre className="text-mono mt-0">{this.props.details.res}</pre>
        </div>
      )
    } else if (['new item', 'edited item'].includes(type)) {
      return <span className="text-body" dangerouslySetInnerHTML={{__html: this.props.details.snip}}></span>
    } else if (type === "lin") {
      return (
        <p className="sans-serif">{this.props.details.content}</p>
      );
    } else {
      return <span className="text-mono">{'<unknown message type>'}</span>;
    }
  }

  render() {
    return (
      <div className="w-100 pa2 mb3 cf flex">
        <div className="fl mr2">
          <Sigil ship={this.props.msg.aut} size={44} />
        </div>
        <div className="fr" style={{ flexGrow: 1 }}>
          <div className="mb2">
            <p className="sans-serif gray dib mr2">~{this.props.msg.aut}</p>
            <p className="sans-serif gray dib">{moment.unix(this.props.msg.wen).format('hh:mm')}</p>
          </div>
          {this.renderContent(this.props.details.type)}
        </div>
      </div>
    );
  }
}

