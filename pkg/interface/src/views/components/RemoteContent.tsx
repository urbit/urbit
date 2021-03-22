import React, { Component, Fragment } from 'react';
import { BaseAnchor, BaseImage, Box, Button, Text } from '@tlon/indigo-react';
import { hasProvider } from 'oembed-parser';
import EmbedContainer from 'react-oembed-container';
import useSettingsState from '~/logic/state/settings';
import { RemoteContentPolicy } from '~/types/local-update';
import { VirtualContextProps, withVirtual } from "~/logic/lib/virtualContext";
import { IS_IOS } from '~/logic/lib/platform';
import withState from '~/logic/lib/withState';

type RemoteContentProps = VirtualContextProps & {
  url: string;
  text?: string;
  unfold?: boolean;
  renderUrl?: boolean;
  remoteContentPolicy: RemoteContentPolicy;
  imageProps?: any;
  audioProps?: any;
  videoProps?: any;
  oembedProps?: any;
  textProps?: any;
  style?: any;
}

interface RemoteContentState {
  unfold: boolean;
  embed: any | undefined;
  noCors: boolean;
}

const IMAGE_REGEX = new RegExp(/(jpg|img|png|gif|tiff|jpeg|webp|webm|svg)$/i);
const AUDIO_REGEX = new RegExp(/(mp3|wav|ogg)$/i);
const VIDEO_REGEX = new RegExp(/(mov|mp4|ogv)$/i);


class RemoteContent extends Component<RemoteContentProps, RemoteContentState> {
  private fetchController: AbortController | undefined;
  containerRef: HTMLDivElement | null = null;
  private saving = false;
  constructor(props) {
    super(props);
    this.state = {
      unfold: props.unfold || false,
      embed: undefined,
      noCors: false
    };
    this.unfoldEmbed = this.unfoldEmbed.bind(this);
    this.loadOembed = this.loadOembed.bind(this);
    this.wrapInLink = this.wrapInLink.bind(this);
    this.onError = this.onError.bind(this);
  }

  save = () => {
    if(this.saving) {
      return;
    }
    this.saving = true;
    this.props.save();
  };

  restore = () => {
    this.saving = false;
    this.props.restore();
  }

  componentWillUnmount() {
    if(this.saving) {
      this.restore();
    }
    if (this.fetchController) {
      this.fetchController.abort();
    }
  }

  unfoldEmbed(event: Event) {
    event.stopPropagation();
    let unfoldState = this.state.unfold;
    unfoldState = !unfoldState;
    this.save();
    this.setState({ unfold: unfoldState });
    requestAnimationFrame(() => {
      this.restore();
    });
  }


  componentDidUpdate(prevProps, prevState) {
    if(prevState.embed !== this.state.embed) {
      //console.log('remotecontent: restoring');
      //prevProps.shiftLayout.restore();
    }
    const { url } = this.props;
    if(url !== prevProps.url && (IMAGE_REGEX.test(url) || AUDIO_REGEX.test(url) || VIDEO_REGEX.test(url))) {
      this.save();
    };

  }

  componentDidMount() {
  }

  onLoad = () => {
    window.requestAnimationFrame(() => {
      const { restore } = this;
      restore();
    });

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
      if (error.name === 'AbortError')
return;
      this.setState({ embed: 'error' });
    });
  }

  wrapInLink(contents) {
    const { style } = this.props;
    return (<BaseAnchor
      href={this.props.url}
      flexShrink={0}
      style={{ color: 'inherit', textDecoration: 'none', ...style }}
      className={`word-break-all ${(typeof contents === 'string') ? 'bb' : ''}`}
      target="_blank"
      width="100%"
      rel="noopener noreferrer"
            >
      {contents}
    </BaseAnchor>);
  }

  onError(e: Event) {
    this.restore();
    this.setState({ noCors: true });
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
      ...props
    } = this.props;
    const { onLoad } = this;
    const { noCors } = this.state;
    const isImage = IMAGE_REGEX.test(url);
    const isAudio = AUDIO_REGEX.test(url);
    const isVideo = VIDEO_REGEX.test(url);
    const isOembed = hasProvider(url);

    if (isImage && remoteContentPolicy.imageShown) {
      return this.wrapInLink(
        <BaseImage
          {...(noCors ? {} : { crossOrigin: "anonymous" })}
          flexShrink={0}
          src={url}
          style={style}
          onLoad={onLoad}
          onError={this.onError}
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
            onLoad={onLoad}
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
            flexShrink={0}
            style={{ cursor: 'pointer' }}
                                                                               >
            {this.state.unfold ? 'collapse' : 'expand'}
          </Button> : null}
          <Box
            mb='2'
            width='100%'
            flexShrink={0}
            display={this.state.unfold ? 'block' : 'none'}
            className='embed-container'
            style={style}
            flexShrink={0}
            onLoad={this.onLoad}
            {...oembedProps}
            {...props}
          >
            {this.state.embed && this.state.embed.html && this.state.unfold
            ? <EmbedContainer markup={this.state.embed.html}>
              <div className="embed-container" ref={(el) => {
                this.onLoad();
 this.containerRef = el;
}}
                dangerouslySetInnerHTML={{ __html: this.state.embed.html }}
              ></div>
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

export default withState(withVirtual(RemoteContent), [[useSettingsState, ['remoteContentPolicy']]]);
