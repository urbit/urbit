import React, { Component } from 'react';
import { Route, Link } from "react-router-dom";
import { makeRoutePath, cite } from "../../lib/util";
import moment from "moment";

export class LinkPreview extends Component {
  constructor(props) {
    super(props);
    this.state = {
      timeSinceLinkPost: this.getTimeSinceLinkPost(),
      embed: ""
    };
  }

  componentDidUpdate(prevProps) {
    if (prevProps !== this.props) {
      if (this.state.timeSinceLinkPost === "") {
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

    // check for soundcloud for fetching embed
    let soundcloudRegex = new RegExp('' +
      /(https?:\/\/(?:www.)?soundcloud.com\/[\w-]+\/?(?:sets\/)?[\w-]+)/.source
    );

    let isSoundcloud = soundcloudRegex.exec(this.props.url);

    if (isSoundcloud && this.state.embed === "") {
      fetch(
        'https://soundcloud.com/oembed?format=json&url=' +
        encodeURIComponent(this.props.url))
        .then((response) => {
          return response.json();
        })
        .then((json) => {
          this.setState({ embed: json.html })
        });
    } else if (!isSoundcloud) {
      this.setState({ embed: "" });
    }
  }

  componentWillUnmount() {
    if (this.updateTimeSinceNewestMessageInterval) {
      clearInterval(this.updateTimeSinceNewestMessageInterval);
      this.updateTimeSinceNewestMessageInterval = null;
    }
  }

  getTimeSinceLinkPost() {
    const time = this.props.time;
    return !!time
      ? moment.unix(time / 1000).from(moment.utc())
      : "";
  }

  render() {
    const { props } = this;

    let URLparser = new RegExp(
      /((?:([\w\d\.-]+)\:\/\/?){1}(?:(www)\.?){0,1}(((?:[\w\d-]+\.)*)([\w\d-]+\.[\w\d]+))){1}(?:\:(\d+)){0,1}((\/(?:(?:[^\/\s\?]+\/)*))(?:([^\?\/\s#]+?(?:.[^\?\s]+){0,1}){0,1}(?:\?([^\s#]+)){0,1})){0,1}(?:#([^#\s]+)){0,1}/
    );

    let hostname = URLparser.exec(props.url);

    if (hostname) {
      hostname = hostname[4];
    }

    let imgMatch = /(jpg|img|png|gif|tiff|jpeg|JPG|IMG|PNG|TIFF|GIF|webp|WEBP|webm|WEBM)$/.exec(
      props.url
    );

    let youTubeRegex = new RegExp(
      "" +
      /(?:https?:\/\/(?:[a-z]+.)?)/.source + // protocol
      /(?:youtu\.?be(?:\.com)?\/)(?:embed\/)?/.source + // short and long-links
        /(?:(?:(?:(?:watch\?)?(?:time_continue=(?:[0-9]+))?.+v=)?([a-zA-Z0-9_-]+))(?:\?t\=(?:[0-9a-zA-Z]+))?)/.source // id
    );

    let ytMatch = youTubeRegex.exec(props.url);

    let embed = "";

    if (imgMatch) {
      embed = <a href={props.url}
                target="_blank"
                style={{width: "max-content"}}>
        <img src={props.url} style={{maxHeight: "500px", maxWidth: "100%"}}/>
      </a>
    }

    if (ytMatch) {
      embed = (
        <iframe
          ref="iframe"
          width="560"
          height="315"
          src={`https://www.youtube.com/embed/${ytMatch[1]}`}
          frameBorder="0"
          allow="picture-in-picture, fullscreen"></iframe>
      );
    }

    let nameClass = props.nickname ? "inter" : "mono";

    return (
      <div className="pb6 w-100">
        <div
          className={"w-100 tc " + (ytMatch ? "embed-container" : "")}>
          {embed || <div dangerouslySetInnerHTML={{__html: this.state.embed}}/>}
        </div>
        <div className="flex flex-column ml2 pt6 flex-auto">
          <a href={props.url} className="w-100 flex" target="_blank">
            <p className="f8 truncate">
              {props.title}
            </p>
            <span className="gray2 ml2 f8 dib v-btm flex-shrink-0">{hostname} ↗</span>
          </a>
          <div className="w-100 pt1">
            <span className={"f9 pr2 white-d dib " + nameClass}
            title={props.ship}>
              {props.nickname ? props.nickname : cite(props.ship)}
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