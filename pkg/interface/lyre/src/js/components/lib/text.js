import React, { Component } from 'react';
import classnames from 'classnames';
import { parseAllStyles } from '/lib/style-parse';

export class Text extends Component {
  constructor(props) {
    super(props);
  }

  render() {
    let style = parseAllStyles(this.props.style);

    let lines = this.props.body.split('\n').map((line, i) => {
      return (
        <p key={i} style={style}>
          {line}
        </p>
      );
    });
    return (
      <div style={style}>
        {lines}
      </div>
    );
  }
}
