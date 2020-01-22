import React, { Component } from 'react'
import moment from 'moment';

import { Sigil } from '/components/lib/icons/sigil';
import { Route, Link } from 'react-router-dom';

export class LinkItem extends Component {
  constructor(props) {
    super(props);
    this.state = {
      timeSinceLinkPost: this.getTimeSinceLinkPost()
    };
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

  render() {

    let props = this.props;

    let mono = (props.nickname) ? "inter white-d" : "mono white-d";

    let URLparser = new RegExp(/((?:([\w\d\.-]+)\:\/\/?){1}(?:(www)\.?){0,1}(((?:[\w\d-]+\.)*)([\w\d-]+\.[\w\d]+))){1}(?:\:(\d+)){0,1}((\/(?:(?:[^\/\s\?]+\/)*))(?:([^\?\/\s#]+?(?:.[^\?\s]+){0,1}){0,1}(?:\?([^\s#]+)){0,1})){0,1}(?:#([^#\s]+)){0,1}/);

    let hostname = URLparser.exec(props.url);

    if (hostname) {
      hostname = hostname[4];
    }

    let comments = props.comments + " comment" + ((props.comments === 1) ? "" : "s");
    
    return (
      <div className="w-100 pv3 flex">
        <Sigil
          ship={"~" + props.ship}
          size={36}
          color={"#" + props.color}
            />
        <div className="flex flex-column ml2">
          <a href={props.url}
          className="w-100 flex"
          target="_blank">
            <p className="f8 truncate">{props.title}
              <span className="gray2 dib truncate-m mw4-m v-btm ml2">{hostname} ↗</span>
            </p>
          </a>
          <div className="w-100 pt1">
            <span className={"f9 pr2 v-mid " + mono}>{(props.nickname) 
            ? props.nickname 
            : "~" + props.ship}</span>
          <span className="f9 inter gray2 pr3 v-mid">{this.state.timeSinceLinkPost}</span>
          <Link to=
          {"/~link" + props.popout + "/" + props.channel + "/" + props.page + "/" + props.index}
          className="v-top">
            <span className="f9 inter gray2">
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
