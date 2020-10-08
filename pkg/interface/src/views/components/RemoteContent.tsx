import React, { PureComponent, Fragment } from 'react';
import { LocalUpdateRemoteContentPolicy } from "~/types/local-update";
import { Button } from '@tlon/indigo-react';
import { hasProvider } from 'oembed-parser';
import EmbedContainer from 'react-oembed-container';
import { memoize } from 'lodash';

interface RemoteContentProps {
  url: string;
  remoteContentPolicy: LocalUpdateRemoteContentPolicy;
  unfold?: boolean;
  renderUrl?: boolean;
  imageProps?: any;
  audioProps?: any;
  videoProps?: any;
  oembedProps?: any;
  style?: any;
  onLoad?(): void;
}

interface RemoteContentState {
  unfold: boolean;
  embed: any | undefined;
}

const IMAGE_REGEX = new RegExp(/(jpg|img|png|gif|tiff|jpeg|webp|webm|svg)$/i);
const AUDIO_REGEX = new RegExp(/(mp3|wav|ogg)$/i);
const VIDEO_REGEX = new RegExp(/(mov|mp4|ogv)$/i);

const memoizedFetch = memoize(fetch);

export default class RemoteContent extends PureComponent<RemoteContentProps, RemoteContentState> {
  private fetchController: AbortController | undefined;
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

  componentWillUnmount() {
    if (this.fetchController) {
      this.fetchController.abort();
    }
  }

  unfoldEmbed() {
    let unfoldState = this.state.unfold;
    unfoldState = !unfoldState;
    this.setState({ unfold: unfoldState });
    setTimeout(this.props.onLoad, 500);
  }

  loadOembed() {
    this.fetchController = new AbortController();
    memoizedFetch(`https://noembed.com/embed?url=${this.props.url}`, {
      signal: this.fetchController.signal
    })
    .then(response => response.clone().json())
    .then((result) => {
      this.setState({ embed: result });
    }).catch((error) => {
      if (error.name === 'AbortError') return;
      this.setState({ embed: 'error' });
    });
  }

  wrapInLink(contents) {
    return (<a
      href={this.props.url}
      style={{ color: 'inherit' }}
      className={`word-break-all ${(typeof contents === 'string') ? 'bb' : ''}`}
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
      style = {},
      onLoad = () => {},
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
          style={style}
          onLoad={onLoad}
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
            style={style}
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
            style={style}
            onLoad={onLoad}
            {...videoProps}
            {...props}
          />
        </>
      );
    } else if (isOembed && remoteContentPolicy.oembedShown) {
      if (!this.state.embed || this.state.embed?.html === '') {
        this.loadOembed();
      }

      return (
        <Fragment>
          {renderUrl ? this.wrapInLink(this.state.embed && this.state.embed.title ? this.state.embed.title : url) : null}
          {this.state.embed !== 'error' && this.state.embed?.html && !unfold ? <Button
            display='inline-flex'
            border={1}
            height={3}
            ml={1}
            onClick={this.unfoldEmbed}
            style={{ cursor: 'pointer' }}
          >
            {this.state.unfold ? 'collapse' : 'expand'}
          </Button> : null}
          <div
            className={'embed-container mb2 w-100 w-75-l w-50-xl ' + (this.state.unfold ? 'db' : 'dn')}
            style={style}
            onLoad={onLoad}
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
