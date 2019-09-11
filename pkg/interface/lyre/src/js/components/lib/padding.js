import React, { Component } from 'react';
import classnames from 'classnames';
import { Dom } from '/components/dom';

export class Padding extends Component {
  constructor(props) {
    super(props);
  }

  render() {
    return (
      <div style={{marginTop: this.props.top,
                   marginBottom: this.props.bottom,
                   marginLeft: this.props.left,
                   marginRight: this.props.right,
            }}>
        <Dom body={this.props.body} api={this.props.api}/>
      </div>
    );
  }
}
