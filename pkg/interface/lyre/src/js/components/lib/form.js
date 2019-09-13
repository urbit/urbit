import React, { Component } from 'react';
import classnames from 'classnames';
import { Dom } from '/components/dom';

export class Form extends Component {
  constructor(props) {
    super(props);
  }

  render() {
    return (
      <form id="myForm" app={this.props.app} mark={this.props.mark}>
        <Dom body={this.props.body} api={this.props.api}/>
      </form>
    );
  }
}
