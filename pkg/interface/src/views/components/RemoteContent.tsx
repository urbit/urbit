import React, { PureComponent, Fragment } from 'react';
import { LocalUpdateRemoteContentPolicy } from "~/types/local-update";
import { BaseAnchor, BaseImage, Box, Button, Text } from '@tlon/indigo-react';
import { hasProvider } from 'oembed-parser';
import EmbedContainer from 'react-oembed-container';
import { memoize } from 'lodash';

interface RemoteContentProps {
  url: string;
  text?: string;
  remoteContentPolicy: LocalUpdateRemoteContentPolicy;
  unfold?: boolean;
  renderUrl?: boolean;
  imageProps?: any;
  audioProps?: any;
  videoProps?: any;
  oembedProps?: any;
  textProps?: any;
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

  unfoldEmbed(event: Event) {
    event.stopPropagation();
    let unfoldState = this.state.unfold;
    unfoldState = !unfoldState;
    this.setState({ unfold: unfoldState });
    setTimeout(this.props.onLoad, 500);
  }

  loadOembed() {
    this.fetchController = new AbortController();
    fetch(`https://noembed.com/embed?url=${this.props.url}`, {
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
    const { style } = this.props;
    return (<BaseAnchor
      href={this.props.url}
      style={{ color: 'inherit', textDecoration: 'none', ...style }}
      className={`word-break-all ${(typeof contents === 'string') ? 'bb' : ''}`}
      target="_blank"
      width="100%"
      rel="noopener noreferrer"
    >
      {contents}
    </BaseAnchor>);
  }

  render() {
    const {
      remoteContentPolicy,
      url,
      text,
      unfold = false,
      renderUrl = true,
      imageProps = {},
      audioProps = {},
      videoProps = {},
      oembedProps = {},
      textProps = {},
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
        <BaseImage
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
          {renderUrl
            ? this.wrapInLink(<Text {...textProps}>{text || url}</Text>)
            : null}
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
          {renderUrl
            ? this.wrapInLink(<Text {...textProps}>{text || url}</Text>)
            : null}
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
          {renderUrl
            ? this.wrapInLink(<Text {...textProps}>{(this.state.embed && this.state.embed.title)
              ? this.state.embed.title
              : (text || url)}</Text>)
            : null}
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
          <Box
            mb='2'
            width='100%'
            display={this.state.unfold ? 'block' : 'none'}
            className='embed-container'
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
          </Box>
        </Fragment>
      );
    } else {
      return renderUrl
        ? this.wrapInLink(<Text {...textProps}>{text || url}</Text>) 
        : null;
    }
  }
}
