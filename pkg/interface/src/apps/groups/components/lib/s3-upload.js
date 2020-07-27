import React, { Component } from 'react'
import S3Client from '../../../../lib/s3';

export class S3Upload extends Component {

  constructor(props) {
    super(props);
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

    this.s3.upload(bucket, file.name, file).then((data) => {
      if (!data || !('Location' in data)) {
        return;
      }
      this.props.uploadSuccess(data.Location);
    }).catch((err) => {
      console.error(err);
      this.props.uploadError(err);
    });
  }

  onClick() {
    if (!this.inputRef.current) { return; }
    this.inputRef.current.click();
  }

  render() {
    const { props } = this;
    if (!this.isReady(props.credentials, props.configuration)) {
      return <div></div>;
    } else {
      let classes = !!props.className ?
        "pointer " + props.className : "pointer";
      return (
        <div className={classes}>
          <input className="dn"
                 type="file"
                 id="fileElement"
                 ref={this.inputRef}
                 accept="image/*"
                 onChange={this.onChange.bind(this)} />
          <img className="invert-d"
               src="/~landscape/img/ImageUpload.png"
               width="32"
               height="32"
               onClick={this.onClick.bind(this)} />
        </div>
      );
    }
  }
}

