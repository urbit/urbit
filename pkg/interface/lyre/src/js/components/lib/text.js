import React, { Component } from 'react';
import classnames from 'classnames';

export class Text extends Component {
  constructor(props) {
    super(props);
  }

  render() {
    let lines = this.props.body.split('\n').map((line, i) => {
      return (
        <p className="label-regular-mono" key={i} style={{whiteSpace:"pre"}}>
          {line}
        </p>
      );
    });
    return (
      <div>
        {lines}
      </div>
    );
  }
}
