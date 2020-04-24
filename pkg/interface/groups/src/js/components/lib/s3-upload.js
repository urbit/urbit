import React, { Component } from 'react'
import S3Client from '/lib/s3';

export class S3Upload extends Component {

  constructor(props) {
    super(props);
    console.log(props);
    let credentials = props.s3Credentials.credentials;
    let configuration = props.s3Credentials.configuration;
    this.s3 = new S3Client();
    this.s3.setCredentials(
      credentials.endpoint,
      credentials.accessKeyId,
      credentials.secretAccessKey
    );
    window.s3 = this.s3;
  }

  componentDidUpdate(prevProps) {
    const { props } = this;
    console.log(props);
  }

  render() {
    const { props } = this;
    return <div></div>;
  }
}

