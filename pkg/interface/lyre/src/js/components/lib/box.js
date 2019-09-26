import React, { Component } from 'react';
import classnames from 'classnames';
import { Dom } from '/components/dom';
import { parseFlex, parseLayout, parseTypography, parseBGColor, parseBorder } from '/lib/style-parse';

export class Box extends Component {
  constructor(props) {
    super(props);

  }

  render() {
    let style = this.props.style.reduce((acc, el) => {
      let res = parseFlex(acc, el);
      if (res === acc) {
        res = parseLayout(acc, el);
      }
      if (res === acc) {
        res = parseTypography(acc, el);
      }
      if (res === acc) {
        res = parseBGColor(acc, el);
      }
      if (res === acc) {
        res = parseBorder(acc, el);
      }
      return res;
    }, {});

    return (
      <div style={style}>
        <Dom body={this.props.body} api={this.props.api}/>
      </div>
    );
  }
}
