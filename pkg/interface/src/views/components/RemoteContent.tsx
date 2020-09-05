import React, { Component, Fragment } from 'react';
import { LocalUpdateRemoteContentPolicy } from "~/types/local-update";
import { Button } from '@tlon/indigo-react';
import { hasProvider } from 'oembed-parser';
import EmbedContainer from 'react-oembed-container';

interface RemoteContentProps {
  url: string;
  remoteContentPolicy: LocalUpdateRemoteContentPolicy;
  unfold: boolean;
  renderUrl: boolean;
  imageProps: any;
  audioProps: any;
  videoProps: any;
  oembedProps: any;
}

interface RemoteContentState {
  unfold: boolean;
  embed: any | undefined;
}

const IMAGE_REGEX = new RegExp(/(jpg|img|png|gif|tiff|jpeg|webp|webm|svg)$/i);
const AUDIO_REGEX = new RegExp(/(mp3|wav|ogg)$/i);
const VIDEO_REGEX = new RegExp(/(mov|mp4|ogv)$/i);

export default class RemoteContent extends Component<RemoteContentProps, RemoteContentState> {
  constructor(props) {
    super(props);
    this.state = {
      unfold: props.unfold || false,
      embed: undefined
    };
    this.unfoldEmbed = this.unfoldEmbed.bind(this);
    this.loadOembed = this.loadOembed.bind(this);
    this.wrapInLink = this.wrapInLink.bind(this);
  }

  unfoldEmbed() {
    let unfoldState = this.state.unfold;
    unfoldState = !unfoldState;
    this.setState({ unfold: unfoldState });
  }

  loadOembed() {
    fetch(`https://noembed.com/embed?url=${this.props.url}`)
    .then(response => response.json())
    .then((result) => {
      this.setState({ embed: result });
    }).catch((error) => {
      this.setState({ embed: 'error' });
      console.log('error fetching oembed', error);
    });
  }

  wrapInLink(contents) {
    return (<a
      href={this.props.url}
      className={`word-break-all ${(typeof contents === 'string') ? 'bb b--white-d b--black' : ''}`}
      target="_blank"
      rel="noopener noreferrer"
    >
      {contents}
    </a>);
  }

  render() {
    const {
      remoteContentPolicy,
      url,
      unfold = false,
      renderUrl = true,
      imageProps = {},
      audioProps = {},
      videoProps = {},
      oembedProps = {},
      ...props
    } = this.props;
    const isImage = IMAGE_REGEX.test(url);
    const isAudio = AUDIO_REGEX.test(url);
    const isVideo = VIDEO_REGEX.test(url);
    const isOembed = hasProvider(url);

    if (isImage && remoteContentPolicy.imageShown) {
      return this.wrapInLink(
        <img
          src={url}
          {...imageProps}
          {...props}
        />
      );
    } else if (isAudio && remoteContentPolicy.audioShown) {
      return (
        <>
          {renderUrl ? this.wrapInLink(url) : null}
          <audio
            controls
            className="db"
            src={url}
            {...audioProps}
            {...props}
          />
        </>
      );
    } else if (isVideo && remoteContentPolicy.videoShown) {
      return (
        <>
          {renderUrl ? this.wrapInLink(url) : null}
          <video
            controls
            className="db"
            src={url}
            {...videoProps}
            {...props}
          />
        </>
      );
    } else if (isOembed && remoteContentPolicy.oembedShown) {
      this.loadOembed();
      return (
        <Fragment>
          {renderUrl ? this.wrapInLink(this.state.embed && this.state.embed.title ? this.state.embed.title : url) : null}
          {this.state.embed !== 'error' && !unfold ? <Button
            border={1}
            style={{ display: 'inline-flex', height: '1.66em' }} // Height is hacked to line-height until Button supports proper size
            ml={1}
            onClick={this.unfoldEmbed}
          >
            {this.state.unfold ? 'collapse' : 'expand'}
          </Button> : null}
          <div
            className={'embed-container mb2 w-100 w-75-l w-50-xl ' + (this.state.unfold ? 'db' : 'dn')}
            {...oembedProps}
            {...props}
          >
            {this.state.embed && this.state.embed.html && this.state.unfold
            ? <EmbedContainer markup={this.state.embed.html}>
              <div dangerouslySetInnerHTML={{__html: this.state.embed.html}}></div>
            </EmbedContainer>
            : null}
          </div>
        </Fragment>
      );
    } else {
      return renderUrl ? this.wrapInLink(url) : null;
    }
  }
}