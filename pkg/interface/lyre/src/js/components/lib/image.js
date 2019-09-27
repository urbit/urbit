import React, { Component } from 'react';
import classnames from 'classnames';
import { parseAllStyles } from '/lib/style-parse';

export class Image extends Component {
  constructor(props) {
    super(props);
  }

  render() {
    return (
      <img src={`data:image/png;base64,${this.props.data}`}>
      </img>
    );
  }
}
