import { BaseAnchor, BaseImage, Box, Icon, Row, Text } from '@tlon/indigo-react';
import { hasProvider } from 'oembed-parser';
import React, { Component, Fragment } from 'react';
import EmbedContainer from 'react-oembed-container';
import styled from 'styled-components';
import { VirtualContextProps, withVirtual } from '~/logic/lib/virtualContext';
import withState from '~/logic/lib/withState';
import useSettingsState from '~/logic/state/settings';
import { RemoteContentPolicy } from '~/types/local-update';

export type RemoteContentProps = VirtualContextProps & {
  url: string;
  text?: string;
  unfold?: boolean;
  renderUrl?: boolean;
  remoteContentPolicy?: RemoteContentPolicy;
  imageProps?: any;
  audioProps?: any;
  videoProps?: any;
  oembedProps?: any;
  textProps?: any;
  style?: any;
  transcluded?: any;
  className?: string;
  tall?: boolean;
}

interface RemoteContentState {
  unfold: boolean;
  embed: any | undefined;
  noCors: boolean;
  showArrow: boolean;
}

const IMAGE_REGEX = new RegExp(/(jpg|img|png|gif|tiff|jpeg|webp|webm|svg)$/i);
const AUDIO_REGEX = new RegExp(/(mp3|wav|ogg)$/i);
const VIDEO_REGEX = new RegExp(/(mov|mp4|ogv)$/i);

const TruncatedText = styled(Text)`
  white-space: pre;
  text-overflow: ellipsis;
  overflow: hidden;
  min-width: 0;
`;

class RemoteContent extends Component<RemoteContentProps, RemoteContentState> {
  private fetchController: AbortController | undefined;
  containerRef: HTMLDivElement | null = null;
  private saving = false;
  private isOembed = false;
  constructor(props) {
    super(props);
    this.state = {
      unfold: props.unfold || false,
      embed: undefined,
      noCors: false,
      showArrow: false
    };
    this.unfoldEmbed = this.unfoldEmbed.bind(this);
    this.loadOembed = this.loadOembed.bind(this);
    this.wrapInLink = this.wrapInLink.bind(this);
    this.onError = this.onError.bind(this);
    this.toggleArrow = this.toggleArrow.bind(this);
    this.isOembed = hasProvider(props.url);
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
      // console.log('remotecontent: restoring');
      // prevProps.shiftLayout.restore();
    }
    const { url } = this.props;
    if(url !== prevProps.url && (IMAGE_REGEX.test(url) || AUDIO_REGEX.test(url) || VIDEO_REGEX.test(url))) {
      this.save();
    }
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

  wrapInLink(contents, textOnly = false, unfold = false, unfoldEmbed = null, embedContainer = null, flushPadding = false, noOp = false) {
    const { style, tall = false } = this.props;
    const maxWidth = tall ? '100%' : 'min(500px, 100%)';
    return (
      <Box borderRadius={1} backgroundColor="washedGray" maxWidth={maxWidth}>
      <Row
        alignItems="center"
        gapX={1}
      >
        { textOnly && (<Icon ml={2} display="block" icon="ArrowExternal" />)}
        { !textOnly && unfoldEmbed && (
          <Icon
            ml={2}
            display='block'
            onClick={unfoldEmbed}
            icon={unfold ? 'TriangleSouth' : 'TriangleEast'}
          />
          )}
        <BaseAnchor
          display="flex"
          p={flushPadding ? 0 : 2}
          onClick={(e) => {
 noOp ? e.preventDefault() : e.stopPropagation();
}}
          href={this.props.url}
          whiteSpace="nowrap"
          overflow="hidden"
          textOverflow="ellipsis"
          minWidth={0}
          width={textOnly ? 'calc(100% - 24px)' : 'fit-content'}
          maxWidth={maxWidth}
          style={{ color: 'inherit', textDecoration: 'none', ...style }}
          target="_blank"
          rel="noopener noreferrer"
          cursor={noOp ? 'default' : 'pointer'}
        >
        {contents}
      </BaseAnchor>
    </Row>
    {embedContainer}
    </Box>
    );
  }

  onError(e: Event) {
    this.restore();
    this.setState({ noCors: true });
  }

  toggleArrow() {
    this.setState({ showArrow: !this.state.showArrow });
  }

  render() {
    const {
      remoteContentPolicy,
      url,
      text,
      transcluded,
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

    const isTranscluded = () => {
      return transcluded;
    };

    if (isImage && remoteContentPolicy.imageShown) {
      return this.wrapInLink(
        <Box
          position='relative'
          onMouseEnter={this.toggleArrow}
          onMouseLeave={this.toggleArrow}
        >
          <BaseAnchor
            position='absolute'
            top={2}
            right={2}
            display={this.state.showArrow ? 'block' : 'none'}
            target='_blank'
            rel='noopener noreferrer'
            onClick={(e) => {
              e.stopPropagation();
            }}
            href={url}
          >
            <Box
              backgroundColor='white'
              padding={2}
              borderRadius='50%'
              display='flex'
            >
              <Icon icon='ArrowNorthEast' />
            </Box>
          </BaseAnchor>
          <BaseImage
            {...(noCors ? {} : { crossOrigin: 'anonymous' })}
            referrerPolicy='no-referrer'
            flexShrink={0}
            src={url}
            style={style}
            onLoad={onLoad}
            onError={this.onError}
            height='100%'
            width='100%'
            objectFit='contain'
            borderRadius={2}
            {...imageProps}
            {...props}
          />
        </Box>,
        false,
        false,
        null,
        null,
        true,
        isTranscluded()
      );
    } else if (isAudio && remoteContentPolicy.audioShown) {
      return (
        <>
          {renderUrl
            ? this.wrapInLink(
              <TruncatedText {...textProps}>{url}</TruncatedText>,
              false,
              this.state.unfold,
              this.unfoldEmbed,
            <audio
              onClick={(e) => {
 e.stopPropagation();
}}
              controls
              className={this.state.unfold ? 'db' : 'dn'}
              src={url}
              style={style}
              onLoad={onLoad}
              objectFit="contain"
              height="100%"
              width="100%"
              {...audioProps}
              {...props}
            />)
            : null}
        </>
      );
    } else if (isVideo && remoteContentPolicy.videoShown) {
      return (
        <>
          {renderUrl
            ? this.wrapInLink(
              <TruncatedText {...textProps}>{url}</TruncatedText>,
              false,
              this.state.unfold,
              this.unfoldEmbed,
              <video
                onClick={(e) => {
 e.stopPropagation();
}}
                controls
                className={this.state.unfold ? 'db' : 'dn pa2'}
                src={url}
                style={style}
                onLoad={onLoad}
                objectFit="contain"
                height="100%"
                width="100%"
                {...videoProps}
                {...props}
              />)
            : null}
        </>
      );
    } else if (this.isOembed && remoteContentPolicy.oembedShown) {
      if (!this.state.embed || this.state.embed?.html === '') {
        this.loadOembed();
      }
     const renderEmbed = !(this.state.embed !== 'error' && this.state.embed?.html);
     const embed = <Box
            mb={2}
            width='100%'
            flexShrink={0}
            display={this.state.unfold ? 'block' : 'none'}
            className='embed-container'
            style={style}
            onLoad={this.onLoad}
            {...oembedProps}
            {...props}
                   >
            <TruncatedText
            display={(renderUrl && this.state.embed?.title && this.state.embed.title !== url) ? 'inline-block' : 'none'}
            fontWeight='bold' width='100%'
            >
              {this.state.embed?.title}
            </TruncatedText>
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
          </Box>;

      return (
        <Fragment>
          {renderUrl
            ? this.wrapInLink(
            <TruncatedText {...textProps}>{url}</TruncatedText>,
            renderEmbed,
            this.state.unfold,
            this.unfoldEmbed,
            embed
          ) : embed}
        </Fragment>
      );
    } else {
      return renderUrl
        ? this.wrapInLink(<TruncatedText {...textProps}>{text || url}</TruncatedText>, true)
        : null;
    }
  }
}

export default withState(withVirtual(RemoteContent), [[useSettingsState, ['remoteContentPolicy']]]) as React.ComponentType<Omit<RemoteContentProps, 'save' | 'restore' | 'remoteContentPolicy'> & { ref?: any }>;
