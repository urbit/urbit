import React, { Component } from 'react';
import classnames from 'classnames';
import { Dom } from '/components/dom';

export class Size extends Component {
  constructor(props) {
    super(props);
  }

  render() {
    return (
      <div style={{width: this.props.width, height: this.props.height}}>
        <Dom body={this.props.body} api={this.props.api}/>
      </div>
    );
  }
}
