import React, { Component } from 'react';

import { S3Upload } from '~/views/components/s3-upload';
import { Spinner } from '~/views/components/Spinner';
import { Icon } from "@tlon/indigo-react";

export class LinkSubmit extends Component {
  constructor() {
    super();
    this.state = {
      linkValue: '',
      linkTitle: '',
      linkValid: false,
      submitFocus: false,
      disabled: false
    };
    this.setLinkValue = this.setLinkValue.bind(this);
    this.setLinkTitle = this.setLinkTitle.bind(this);
  }

  onClickPost() {
    const link = this.state.linkValue;
    const title = this.state.linkTitle
      ? this.state.linkTitle
      : this.state.linkValue;
      this.setState({ disabled: true });
    this.props.api.links.postLink(this.props.resourcePath, link, title).then((r) => {
      this.setState({
        disabled: false,
        linkValue: '',
        linkTitle: '',
        linkValid: false
      });
    });
  }

  setLinkValid(link) {
    const URLparser = new RegExp(
      /((?:([\w\d\.-]+)\:\/\/?){1}(?:(www)\.?){0,1}(((?:[\w\d-]+\.)*)([\w\d-]+\.[\w\d]+))){1}(?:\:(\d+)){0,1}((\/(?:(?:[^\/\s\?]+\/)*))(?:([^\?\/\s#]+?(?:.[^\?\s]+){0,1}){0,1}(?:\?([^\s#]+)){0,1})){0,1}(?:#([^#\s]+)){0,1}/
    );

    const validURL = URLparser.exec(link);

    if (!validURL) {
      const checkProtocol = URLparser.exec('http://' + link);
      if (checkProtocol) {
        this.setState({ linkValid: true });
        this.setState({ linkValue: 'http://' + link });
      } else {
        this.setState({ linkValid: false });
      }
    } else if (validURL) {
      this.setState({ linkValid: true });
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

  render() {
    console.log('s3', this.props.s3);
    const activeClasses = (this.state.linkValid && !this.state.disabled)
      ? 'green2 pointer' : 'gray2';

    const focus = (this.state.submitFocus)
      ? 'b--black b--white-d'
      : 'b--gray4 b--gray2-d';

    return (
      <div className={'relative ba br1 w-100 mb6 ' + focus}>
        <input
          type="url"
          className="pl2 bg-gray0-d white-d w-100 f8 mt2 pt2"
          placeholder="Paste link here"
          onChange={this.setLinkValue}
          onBlur={() => this.setState({ submitFocus: false })}
          onFocus={() => this.setState({ submitFocus: true })}
          spellCheck="false"
          rows={1}
          onKeyPress={(e) => {
            if (e.key === 'Enter') {
              e.preventDefault();
              this.onClickPost();
            }
          }}
          value={this.state.linkValue}
        />
        <S3Upload
          className="fr pr3 absolute top-1 right-0"
          configuration={this.props.s3.configuration}
          credentials={this.props.s3.credentials}
          uploadSuccess={this.uploadSuccess.bind(this)}
          uploadError={this.uploadError.bind(this)}
        ><span className="green2 f8">Upload File</span></S3Upload>
        <input
          type="text"
          className="pl2 bg-gray0-d white-d w-100 f8 mb2 pb2"
          style={{
            resize: 'none',
            height: 40,
            paddingTop: 16
          }}
          placeholder="Enter title"
          onChange={this.setLinkTitle}
          onBlur={() => this.setState({ submitFocus: false })}
          onFocus={() => this.setState({ submitFocus: true })}
          spellCheck="false"
          rows={1}
          onKeyPress={(e) => {
            if (e.key === 'Enter') {
              e.preventDefault();
              this.onClickPost();
            }
          }}
          value={this.state.linkTitle}
        />
        <button
          className={
            'absolute bg-gray0-d f8 ml2 flex-shrink-0 ' + activeClasses
          }
          disabled={!this.state.linkValid || this.state.disabled}
          onClick={this.onClickPost.bind(this)}
          style={{
            bottom: 12,
            right: 8
          }}
        >
          Post
        </button>
        <Spinner awaiting={this.state.disabled} classes="mt3 absolute right-0" text="Posting to collection..." />
      </div>
    );
  }
}

export default LinkSubmit;
