import React, { Component } from 'react';
import { cite } from '~/logic/lib/util';
import moment from 'moment';

import RemoteContent from "~/views/components/RemoteContent";

export class LinkPreview extends Component {
  constructor(props) {
    super(props);
    this.state = {
      timeSinceLinkPost: this.getTimeSinceLinkPost(),
      embed: ''
    };
  }

  componentDidUpdate(prevProps) {
    if (prevProps !== this.props) {
      if (this.state.timeSinceLinkPost === '') {
        this.setState({
          timeSinceLinkPost: this.getTimeSinceLinkPost()
        });
      }
    }
  }

  componentDidMount() {
    this.updateTimeSinceNewestMessageInterval = setInterval(() => {
      this.setState({
        timeSinceLinkPost: this.getTimeSinceLinkPost()
      });
    }, 60000);
  }

  componentWillUnmount() {
    if (this.updateTimeSinceNewestMessageInterval) {
      clearInterval(this.updateTimeSinceNewestMessageInterval);
      this.updateTimeSinceNewestMessageInterval = null;
    }
  }

  getTimeSinceLinkPost() {
    const time = this.props.time;
    return time
      ? moment.unix(time / 1000).from(moment.utc())
      : '';
  }

  render() {
    const { props } = this;

    const embed = (
      <RemoteContent
        unfold={true}
        renderUrl={false}
        url={props.url}
        remoteContentPolicy={props.remoteContentPolicy}
        className="mw-100"
      />
    );

    const URLparser = new RegExp(
      /((?:([\w\d\.-]+)\:\/\/?){1}(?:(www)\.?){0,1}(((?:[\w\d-]+\.)*)([\w\d-]+\.[\w\d]+))){1}(?:\:(\d+)){0,1}((\/(?:(?:[^\/\s\?]+\/)*))(?:([^\?\/\s#]+?(?:.[^\?\s]+){0,1}){0,1}(?:\?([^\s#]+)){0,1})){0,1}(?:#([^#\s]+)){0,1}/
    );

    let hostname = URLparser.exec(props.url);

    if (hostname) {
      hostname = hostname[4];
    }

    const showNickname = props.nickname && !props.hideNicknames;

    const nameClass = showNickname ? 'inter' : 'mono';

    return (
      <div className="pb6 w-100">
        <div
          className={'w-100 tc'}
        >
          {embed}
        </div>
        <div className="flex flex-column ml2 pt6 flex-auto">
          <a href={props.url} className="w-100 flex" target="_blank" rel="noopener noreferrer">
            <p className="f8 truncate">
              {props.title}
            </p>
            <span className="gray2 ml2 f8 dib v-btm flex-shrink-0">{hostname} ↗</span>
          </a>
          <div className="w-100 pt1">
            <span className={'f9 pr2 white-d dib ' + nameClass}
            title={props.ship}
            >
              {showNickname ? props.nickname : cite(props.ship)}
            </span>
            <span className="f9 inter gray2 pr3 dib">
              {this.state.timeSinceLinkPost}
            </span>
              <span className="f9 inter gray2 dib">{props.comments}</span>
          </div>
        </div>
      </div>
    );
  }
}

export default LinkPreview;
