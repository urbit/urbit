import React, { Component } from 'react'
import { Icon } from "@tlon/indigo-react";

import S3Client from '~/logic/lib/s3';
import { Spinner } from './Spinner';
import { S3Credentials, S3Configuration } from '~/types';
import { dateToDa, deSig } from '~/logic/lib/util';

export const SubmitDragger = () => (
  <div
    className="top-0 bottom-0 left-0 right-0 absolute bg-gray5 h-100 w-100 flex items-center justify-center z-999"
    style={{pointerEvents: 'none'}}
  >Drop a file to upload</div>
);

interface S3UploadProps {
  credentials: S3Credentials;
  configuration: S3Configuration;
  uploadSuccess: Function;
  uploadError: Function;
  className?: string;
  accept: string;
}

interface S3UploadState {
  isUploading: boolean;
}

export class S3Upload extends Component<S3UploadProps, S3UploadState> {
  private s3: S3Client;
  public inputRef: React.RefObject<HTMLInputElement>;

  constructor(props) {
    super(props);
    this.state = {
      isUploading: false
    };
    this.s3 = new S3Client();
    this.setCredentials(props.credentials, props.configuration);
    this.inputRef = React.createRef();
  }

  isReady(creds, config): boolean {
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

  componentDidUpdate(prevProps): void {
    const { props } = this;
    if (!props.credentials !== prevProps.credentials || props.configuration !== prevProps.configuration) {
      this.setCredentials(props.credentials, props.configuration);
    }
  }

  setCredentials(credentials, configuration): void {
    if (!this.isReady(credentials, configuration)) { return; }
    this.s3.setCredentials(
      credentials.endpoint,
      credentials.accessKeyId,
      credentials.secretAccessKey
    );
  }

  onChange(): void {
    const { props } = this;
    if (!this.inputRef.current) { return; }
    let files = this.inputRef.current.files;
    if (!files || files.length <= 0) { return; }

    let file = files.item(0);
    if (!file) { return; }
    const fileParts = file.name.split('.');
    const fileName = fileParts.slice(0, -1);
    const fileExtension = fileParts.pop();
    const timestamp = deSig(dateToDa(new Date()));
    let bucket = props.configuration.currentBucket;

    this.setState({ isUploading: true });

    this.s3.upload(bucket, `${window.ship}/${timestamp}-${fileName}.${fileExtension}`, file)
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
      className = '',
      accept = '*',
      children = false
    } = this.props;
    
    if (!this.isReady(credentials, configuration)) {
      return null;
    }

    const display = children || <Icon icon='ArrowNorth' />;
    return (
      <>
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
      </>
    );
  }
}

