import React, { Component } from 'react'
import moment from 'moment';

import { Sigil } from '/components/lib/icons/sigil';
import { Route, Link } from 'react-router-dom';
import { makeRoutePath, cite } from '../../lib/util';

export class LinkItem extends Component {
  constructor(props) {
    super(props);
    this.state = {
      timeSinceLinkPost: this.getTimeSinceLinkPost()
    };
    this.markPostAsSeen = this.markPostAsSeen.bind(this);
  }

  componentDidMount() {
    this.updateTimeSinceNewestMessageInterval = setInterval( () => {
      this.setState({timeSinceLinkPost: this.getTimeSinceLinkPost()});
    }, 60000);
  }

  componentWillUnmount() {
    if (this.updateTimeSinceNewestMessageInterval) {
      clearInterval(this.updateTimeSinceNewestMessageInterval);
      this.updateTimeSinceNewestMessageInterval = null;
    }
  }

  getTimeSinceLinkPost() {
    return !!this.props.timestamp ?
      moment.unix(this.props.timestamp / 1000).from(moment.utc())
      : '';
  }

  markPostAsSeen() {
    api.seenLink(this.props.resourcePath, this.props.url);
  }

  render() {

    let props = this.props;

    let mono = (props.nickname) ? "inter white-d" : "mono white-d";

    let URLparser = new RegExp(/((?:([\w\d\.-]+)\:\/\/?){1}(?:(www)\.?){0,1}(((?:[\w\d-]+\.)*)([\w\d-]+\.[\w\d]+))){1}(?:\:(\d+)){0,1}((\/(?:(?:[^\/\s\?]+\/)*))(?:([^\?\/\s#]+?(?:.[^\?\s]+){0,1}){0,1}(?:\?([^\s#]+)){0,1})){0,1}(?:#([^#\s]+)){0,1}/);

    let hostname = URLparser.exec(props.url);

    const seenState = props.seen
      ? "gray2"
      : "green2 pointer";
    const seenAction = props.seen
      ? ()=>{}
      : this.markPostAsSeen

    if (hostname) {
      hostname = hostname[4];
    }

    let comments = props.comments + " comment" + ((props.comments === 1) ? "" : "s");

    let member = this.props.member || false;
    return (
      <div className="w-100 pv3 flex bg-white bg-gray0-d">
        <Sigil
          ship={"~" + props.ship}
          size={38}
          color={"#" + props.color}
          classes={(member ? "mix-blend-diff" : "")}
            />
        <div className="flex flex-column ml2 flex-auto">
          <a href={props.url}
          className="w-100 flex"
          target="_blank"
          onClick={this.markPostAsSeen}>
            <p className="f8 truncate">{props.title}
            </p>
            <span className="gray2 dib v-btm ml2 f8 flex-shrink-0">{hostname} ↗</span>
          </a>
          <div className="w-100 pt1">
            <span className={"f9 pr2 dib " + mono}
            title={props.ship}>
            {(props.nickname)
              ? props.nickname
              : cite(props.ship)}
            </span>
          <span
            className={seenState + " f9 inter pr3 dib"}
            onClick={this.markPostAsSeen}>
            {this.state.timeSinceLinkPost}
          </span>
          <Link to=
            {makeRoutePath(props.resourcePath, props.popout, props.page, props.url, props.linkIndex)}
          onClick={this.markPostAsSeen}>
            <span className="f9 inter gray2 dib">
                {comments}
              </span>
            </Link>
          </div>
        </div>
      </div>
    )
  }
}

export default LinkItem
