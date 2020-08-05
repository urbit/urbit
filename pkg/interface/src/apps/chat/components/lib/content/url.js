import React, { Component } from 'react';


const IMAGE_REGEX = 
  /(jpg|img|png|gif|tiff|jpeg|JPG|IMG|PNG|TIFF|GIF|webp|WEBP|webm|WEBM|svg|SVG)$/;

const YOUTUBE_REGEX =
  new RegExp(
    String(/(?:https?:\/\/(?:[a-z]+.)?)/.source) // protocol
    + /(?:youtu\.?be(?:\.com)?\/)(?:embed\/)?/.source // short and long-links
    + /(?:(?:(?:(?:watch\?)?(?:time_continue=(?:[0-9]+))?.+v=)?([a-zA-Z0-9_-]+))(?:\?t\=(?:[0-9a-zA-Z]+))?)/.source // id
  );

export default class UrlContent extends Component {
  constructor() {
    super();
    this.state = {
      unfold: false,
      copied: false
    };
    this.unfoldEmbed = this.unfoldEmbed.bind(this);
  }

  unfoldEmbed(id) {
    let unfoldState = this.state.unfold;
    unfoldState = !unfoldState;
    this.setState({ unfold: unfoldState });
    const iframe = this.refs.iframe;
    iframe.setAttribute('src', iframe.getAttribute('data-src'));
  }

  render() {
    const { props } = this;
    const content = props.content;
    const imgMatch = IMAGE_REGEX.exec(props.content.url);
    const ytMatch = YOUTUBE_REGEX.exec(props.content.url);

    let contents = content.url;
    if (imgMatch) {
      contents = (
        <img
          className="o-80-d"
          src={content.url}
          style={{
            width: '50%',
            maxWidth: '250px'
          }}
        ></img>
      );
      return (
        <a className={`f7 lh-copy v-top word-break-all`}
          href={content.url}
          target="_blank"
          rel="noopener noreferrer"
        >
          {contents}
        </a>
      );
    } else if (ytMatch) {
      contents = (
        <div className={'embed-container mb2 w-100 w-75-l w-50-xl ' +
        ((this.state.unfold === true)
          ? 'db' : 'dn')}
        >
        <iframe
          ref="iframe"
          width="560"
          height="315"
          data-src={`https://www.youtube.com/embed/${ytMatch[1]}`}
          frameBorder="0" allow="picture-in-picture, fullscreen"
        >
        </iframe>
        </div>
      );
      return (
        <div>
          <a href={content.url}
            className={`f7 lh-copy v-top bb b--white-d word-break-all`}
            href={content.url}
            target="_blank"
            rel="noopener noreferrer">{content.url}</a>
          <a className="ml2 f7 pointer lh-copy v-top"
             onClick={e => this.unfoldEmbed()}>
            [embed]
          </a>
          {contents}
        </div>
      );
    } else {
      return (
        <a className={`f7 lh-copy v-top bb b--white-d b--black word-break-all`}
          href={content.url}
          target="_blank"
          rel="noopener noreferrer"
        >
          {contents}
        </a>
      );
    }
  }
}
