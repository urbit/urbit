import React, { Component } from 'react'
import { Icon } from "@tlon/indigo-react";

import S3Client from '~/logic/lib/s3';
import { Spinner } from './Spinner';

export class S3Upload extends Component {

  constructor(props) {
    super(props);
    this.state = {
      isUploading: false
    };
    this.s3 = new S3Client();
    this.setCredentials(props.credentials, props.configuration);
    this.inputRef = React.createRef();
  }

  isReady(creds, config) {
    return (
      !!creds &&
      'endpoint' in creds &&
      'accessKeyId' in creds &&
      'secretAccessKey' in creds &&
      creds.endpoint !== '' &&
      creds.accessKeyId !== '' &&
      creds.secretAccessKey !== '' &&
      !!config &&
      'currentBucket' in config &&
      config.currentBucket !== ''
    );
  }

  componentDidUpdate(prevProps) {
    const { props } = this;
    this.setCredentials(props.credentials, props.configuration);
  }

  setCredentials(credentials, configuration) {
    if (!this.isReady(credentials, configuration)) { return; }
    this.s3.setCredentials(
      credentials.endpoint,
      credentials.accessKeyId,
      credentials.secretAccessKey
    );
  }

  getFileUrl(endpoint, filename) {
    return endpoint + '/' + filename;
  }

  onChange() {
    const { props } = this;
    if (!this.inputRef.current) { return; }
    let files = this.inputRef.current.files;
    if (files.length <= 0) { return; }

    let file = files.item(0);
    let bucket = props.configuration.currentBucket;

    this.setState({ isUploading: true });

    this.s3.upload(bucket, file.name, file)
      .then((data) => {
        if (!data || !('Location' in data)) {
          return;
        }
        this.props.uploadSuccess(data.Location);
      })
      .catch((err) => {
        console.error(err);
        this.props.uploadError(err);
      })
      .finally(() => {
        this.setState({ isUploading: false });
      });
  }

  onClick() {
    if (!this.inputRef.current) { return; }
    this.inputRef.current.click();
  }

  render() {
    const {
      credentials,
      configuration,
      className,
      accept = '*',
      children = false
    } = this.props;
    if (!this.isReady(credentials, configuration)) {
      return <div></div>;
    } else {
      let classes = !!className
        ? "pointer " + className
        : "pointer";
      const display = children || <Icon icon='ArrowNorth' />;
      return (
        <div>
          <input
            className="dn"
            type="file"
            id="fileElement"
            ref={this.inputRef}
            accept={accept}
            onChange={this.onChange.bind(this)} />
          {this.state.isUploading
            ? <Spinner awaiting={true} classes={className} />
            : <span className={`pointer ${className}`} onClick={this.onClick.bind(this)}>{display}</span>
          }
          
        </div>
      );
    }
  }
}

