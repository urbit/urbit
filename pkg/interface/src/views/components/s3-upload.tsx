import React, { Component } from 'react'
import { BaseInput, Box, Text, Icon, LoadingSpinner } from "@tlon/indigo-react";

import S3Client from '~/logic/lib/s3';
import { S3Credentials, S3Configuration } from '~/types';
import { dateToDa, deSig } from '~/logic/lib/util';

export const SubmitDragger = () => (
  <Box
    top='0'
    bottom='0'
    left='0'
    right='0'
    position='absolute'
    backgroundColor='white'
    height='100%'
    width='100%'
    display='flex'
    alignItems='center'
    justifyContent='center'
    style={{ pointerEvents: 'none', zIndex: 999 }}
  >
      <Text fontSize='1' color='black'>
        Drop a file to upload
      </Text>
    </Box>
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

    setTimeout(() => {
      if (this.state.isUploading) return;
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
    }, 200);
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
        <BaseInput
          display='none'
          type="file"
          id="fileElement"
          ref={this.inputRef}
          accept={accept}
          onChange={this.onChange.bind(this)} />
        {this.state.isUploading
          ? <LoadingSpinner background="gray" foreground="black" />
          : <Text cursor='pointer' className={className} onClick={this.onClick.bind(this)}>{display}</Text>
        }
      </>
    );
  }
}

