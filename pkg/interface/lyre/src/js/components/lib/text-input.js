import React, { Component } from 'react';
import classnames from 'classnames';
import { Dom } from '/components/dom';
import { parseFlex, parseLayout, parseTypography, parseBGColor, parseBorder } from '/lib/style-parse';

export class TextInput extends Component {
  constructor(props) {
    super(props);
  }

  render() {
    return (
      <input type="text" name={this.props.name}/>
    );
  }
}
