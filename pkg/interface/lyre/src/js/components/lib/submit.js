import React, { Component } from 'react';
import classnames from 'classnames';
import { Dom } from '/components/dom';

export class Submit extends Component {
  constructor(props) {
    super(props);
  }

  render() {
    console.log("submit", this.props);
    return (
      <button onClick={this.props.action}>
        <Dom body={this.props.body}/>
      </button>
    );
  }
}
