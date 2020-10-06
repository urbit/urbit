import React, { Component } from 'react';
import { hasProvider } from 'oembed-parser';

import { S3Upload, SubmitDragger } from '~/views/components/s3-upload';
import { Spinner } from '~/views/components/Spinner';
import { Icon } from "@tlon/indigo-react";
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
    let post = createPost([
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
    );;

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
        }).catch((error) => {/*noop*/});
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
        })
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
    const fire = document.createEvent("HTMLEvents");
    fire.initEvent("change", true, true);
    this.s3Uploader.current?.inputRef.current?.dispatchEvent(fire);
  }

  render() {
    const activeClasses = (this.state.linkValid && !this.state.disabled)
      ? 'green2 pointer' : 'gray2';

    const focus = (this.state.submitFocus)
      ? 'b--black b--white-d'
      : 'b--gray4 b--gray2-d';

    const isS3Ready =
      ( this.props.s3.credentials.secretAccessKey &&
        this.props.s3.credentials.endpoint &&
        this.props.s3.credentials.accessKeyId
      );

    return (
      <div
        className={`flex-shrink-0 relative ba br1 w-100 mb6 ${focus}`}
        onDragEnter={this.onDragEnter.bind(this)}
        onDragOver={e => {
          e.preventDefault();
          if (isS3Ready) {
            this.setState({ dragover: true})
          }
        }}
        onDragLeave={() => this.setState({ dragover: false })}
        onDrop={this.onDrop}
      >
        {this.state.dragover ? <SubmitDragger /> : null}
        <div className="relative">
          {
            ( this.state.linkValue ||
              this.state.urlFocus ||
              this.state.disabled
            ) ? null : (
              isS3Ready ? (
                <span className="gray2 absolute pl2 pt3 pb2 f8"
                      style={{pointerEvents: 'none'}}>
                  Drop or
                  <span className="pointer green2"
                        style={{pointerEvents: 'all'}}
                        onClick={(event) => {
                          if (!this.readyToUpload()) {
                            return;
                          }
                           this.s3Uploader.current.inputRef.current.click();
                        }}> upload </span>
                  a file, or paste a link here
                </span>
              ) : (
                <span className="gray2 absolute pl2 pt3 pb2 f8"
                      style={{pointerEvents: 'none'}}>
                  Paste a link here
                </span>
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
          <input
            type="url"
            className="pl2 w-100 f8 pt3 pb2 white-d bg-transparent"
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
        </div>
          <input
            type="text"
            className="pl2 bg-transparent w-100 f8 white-d linkTitle"
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
          {!this.state.disabled ? <button
            className={
              'bg-transparent f8 flex-shrink-0 pr2 pl2 pt2 pb3 ' + activeClasses
            }
            disabled={!this.state.linkValid || this.state.disabled}
            onClick={this.onClickPost.bind(this)}
            style={{
              bottom: 12,
              right: 8
            }}
          >
            Post link
          </button> : null}
          <Spinner awaiting={this.state.disabled} classes="nowrap flex items-center pr2 pl2 pt2 pb4" style={{flex: '1 1 14rem'}} text="Posting to collection..." />


      </div>
    ) ;
  }
}

export default LinkSubmit;
