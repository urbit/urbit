import React, { Component } from 'react';
import { Box } from '@tlon/indigo-react';

const IMAGE_REGEX = new RegExp(/(jpg|img|png|gif|tiff|jpeg|webp|webm|svg)$/i);

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
    this.iframe.setAttribute('src', this.iframe.dataset.src);
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
            maxWidth: '18rem'
          }}
        ></img>
      );
      return (
        <a className='f7 lh-copy v-top word-break-all'
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
          ref={(el) => {
            this.iframe = el;
          }}
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
            className='f7 lh-copy v-top bb b--white-d word-break-all'
            target="_blank"
            rel="noopener noreferrer"
          >
            {content.url}
          </a>
          <Box
            border={1}
            borderRadius={2}
            display='inline-block'
            fontSize={0}
            style={{ cursor: 'pointer' }}
            opacity={.8}
            px={2}
            ml={1}
            mb={1}
            onClick={e => this.unfoldEmbed()}
          >
            {this.state.unfold ? 'collapse' : 'embed'}
          </Box>
          {contents}
        </div>
      );
    } else {
      return (
        <a className='f7 lh-copy v-top bb b--white-d b--black word-break-all'
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
