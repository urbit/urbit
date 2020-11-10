import React, { Component } from 'react';
import { hasProvider } from 'oembed-parser';

import { S3Upload, SubmitDragger } from '~/views/components/s3-upload';
import { Box, Text, BaseInput, Button } from '@tlon/indigo-react';
import GlobalApi from '~/logic/api/global';
import { S3State } from '~/types';

import { createPost } from '~/logic/api/graph';

interface LinkSubmitProps {
  api: GlobalApi;
  s3: S3State;
  name: string;
  ship: string;
}

interface LinkSubmitState {
  linkValue: string;
  linkTitle: string;
  linkValid: boolean;
  submitFocus: boolean;
  urlFocus: boolean;
  disabled: boolean;
  dragover: boolean;
}

export class LinkSubmit extends Component<LinkSubmitProps, LinkSubmitState> {
  private s3Uploader: React.RefObject<S3Upload>;

  constructor(props) {
    super(props);
    this.state = {
      linkValue: '',
      linkTitle: '',
      linkValid: false,
      submitFocus: false,
      urlFocus: false,
      disabled: false,
      dragover: false
    };
    this.setLinkValue = this.setLinkValue.bind(this);
    this.setLinkTitle = this.setLinkTitle.bind(this);
    this.onDragEnter = this.onDragEnter.bind(this);
    this.onDrop = this.onDrop.bind(this);
    this.onPaste = this.onPaste.bind(this);
    this.uploadFiles = this.uploadFiles.bind(this);
    this.s3Uploader = React.createRef();
  }

  onClickPost() {
    const link = this.state.linkValue;
    const title = this.state.linkTitle
      ? this.state.linkTitle
      : this.state.linkValue;
    this.setState({ disabled: true });

    const parentIndex = this.props.parentIndex || '';
    const post = createPost([
      { text: title },
      { url: link }
    ], parentIndex);

    this.props.api.graph.addPost(
      `~${this.props.ship}`,
      this.props.name,
      post
    ).then((r) => {
      this.setState({
        disabled: false,
        linkValue: '',
        linkTitle: '',
        linkValid: false
      });
    });
  }

  setLinkValid(linkValue) {
    const URLparser = new RegExp(
      /((?:([\w\d\.-]+)\:\/\/?){1}(?:(www)\.?){0,1}(((?:[\w\d-]+\.)*)([\w\d-]+\.[\w\d]+))){1}(?:\:(\d+)){0,1}((\/(?:(?:[^\/\s\?]+\/)*))(?:([^\?\/\s#]+?(?:.[^\?\s]+){0,1}){0,1}(?:\?([^\s#]+)){0,1})){0,1}(?:#([^#\s]+)){0,1}/
    );

    let linkValid = URLparser.test(linkValue);

    if (!linkValid) {
      linkValid = URLparser.test(`http://${linkValue}`);
      if (linkValid) {
        linkValue = `http://${linkValue}`;
      }
    }

    this.setState({ linkValid, linkValue });

    if (linkValid) {
      if (hasProvider(linkValue)) {
        fetch(`https://noembed.com/embed?url=${linkValue}`)
        .then(response => response.json())
        .then((result) => {
          if (result.title && !this.state.linkTitle) {
            this.setState({ linkTitle: result.title });
          }
        }).catch((error) => { /* noop*/ });
      } else if (!this.state.linkTitle) {
        this.setState({
          linkTitle: decodeURIComponent(linkValue
            .split('/')
            .pop()
            .split('.')
            .slice(0, -1)
            .join('.')
            .replace('_', ' ')
            .replace(/\d{4}\.\d{1,2}\.\d{2}\.\.\d{2}\.\d{2}\.\d{2}-/, '')
            )
        });
      }
    }
  }

  setLinkValue(event) {
    this.setState({ linkValue: event.target.value });
    this.setLinkValid(event.target.value);
  }

  setLinkTitle(event) {
    this.setState({ linkTitle: event.target.value });
  }

  uploadSuccess(url) {
    this.setState({ linkValue: url });
    this.setLinkValid(url);
  }

  uploadError(error) {
    //  no-op for now
  }

  readyToUpload(): boolean {
    return Boolean(this.s3Uploader.current && this.s3Uploader.current.inputRef.current);
  }

  onDragEnter() {
    if (!this.readyToUpload()) {
      return;
    }
    this.setState({ dragover: true });
  }

  onDrop(event: DragEvent) {
    this.setState({ dragover: false });
    if (!event.dataTransfer || !event.dataTransfer.files.length) {
      return;
    }
    event.preventDefault();
    this.uploadFiles(event.dataTransfer.files);
  }

  onPaste(event: ClipboardEvent) {
    if (!event.clipboardData || !event.clipboardData.files.length) {
      return;
    }
    event.preventDefault();
    event.stopPropagation();
    this.uploadFiles(event.clipboardData.files);
  }

  uploadFiles(files: FileList) {
    if (!this.readyToUpload()) {
      return;
    }
    this.s3Uploader.current.inputRef.current.files = files;
    const fire = document.createEvent('HTMLEvents');
    fire.initEvent('change', true, true);
    this.s3Uploader.current?.inputRef.current?.dispatchEvent(fire);
  }

  render() {
    const isS3Ready =
      ( this.props.s3.credentials.secretAccessKey &&
        this.props.s3.credentials.endpoint &&
        this.props.s3.credentials.accessKeyId
      );

    return (
      <>
      <Box
        flexShrink='0'
        position='relative'
        border='1px solid'
        borderColor={this.state.submitFocus ? 'black' : 'washedGray'}
        width='100%'
        borderRadius='2'
        onDragEnter={this.onDragEnter.bind(this)}
        onDragOver={(e) => {
          e.preventDefault();
          if (isS3Ready) {
            this.setState({ dragover: true });
          }
        }}
        onDragLeave={() => this.setState({ dragover: false })}
        onDrop={this.onDrop}
      >
        {this.state.dragover ? <SubmitDragger /> : null}
        <Box position='relative'>
          {
            ( this.state.linkValue ||
              this.state.urlFocus ||
              this.state.disabled
            ) ? null : (
              isS3Ready ? (
                <Text gray position='absolute'
                  pl='2' pt='2'
                  pb='2' fontSize='0'
                      style={{ pointerEvents: 'none' }}
                >
                  Drop or
                  <Text cursor='pointer' color='blue'
                        style={{ pointerEvents: 'all' }}
                        onClick={(event) => {
                          if (!this.readyToUpload()) {
                            return;
                          }
                           this.s3Uploader.current.inputRef.current.click();
                        }}
                  > upload </Text>
                  a file, or paste a link here
                </Text>
              ) : (
                <Text gray position='absolute'
                  pl='2' pt='2'
                  pb='2' fontSize='0'
                      style={{ pointerEvents: 'none' }}
                >
                  Paste a link here
                </Text>
              )
            )
          }
          {!this.state.disabled && isS3Ready ? <S3Upload
            ref={this.s3Uploader}
            configuration={this.props.s3.configuration}
            credentials={this.props.s3.credentials}
            uploadSuccess={this.uploadSuccess.bind(this)}
            uploadError={this.uploadError.bind(this)}
            className="dn absolute pt3 pb2 pl2 w-100"
            ></S3Upload> : null}
          <BaseInput
            type="url"
            pl='2'
            width='100%'
            fontSize='0'
            pt='2'
            pb='2'
            color='black'
            backgroundColor='transparent'
            onChange={this.setLinkValue}
            onBlur={() => this.setState({ submitFocus: false, urlFocus: false })}
            onFocus={() => this.setState({ submitFocus: true, urlFocus: true })}
            spellCheck="false"
            onPaste={this.onPaste}
            onKeyPress={(e) => {
              if (e.key === 'Enter') {
                e.preventDefault();
                this.onClickPost();
              }
            }}
            value={this.state.linkValue}
          />
        </Box>
          <BaseInput
            pl='2'
            backgroundColor='transparent'
            width='100%'
            fontSize='0'
            color='black'
            type="text"
            className="linkTitle"
            style={{
              resize: 'none',
              height: 40
            }}
            placeholder="Provide a title"
            onChange={this.setLinkTitle}
            onBlur={() => this.setState({ submitFocus: false })}
            onFocus={() => this.setState({ submitFocus: true })}
            spellCheck="false"
            onKeyPress={(e) => {
              if (e.key === 'Enter') {
                e.preventDefault();
                this.onClickPost();
              }
            }}
            value={this.state.linkTitle}
          />
      </Box>
      <Box mt='2' mb='4'>
        <Button
          flexShrink='0'
          primary
          disabled={!this.state.linkValid || this.state.disabled}
          onClick={this.onClickPost.bind(this)}
        >
            Post link
          </Button>
      </Box>
      </>
    ) ;
  }
}

export default LinkSubmit;
