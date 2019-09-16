import React, { Component } from 'react';
import classnames from 'classnames';
import { Dom } from '/components/dom';

export class Form extends Component {
  constructor(props) {
    super(props);

    this.data = {};
    for (var key in this.props.data) {
      this.data["data-"+key] = this.props.data[key];
    }
  }

  render() {

    return (
      <form data-app={this.props.app}
          data-mark={this.props.mark}
          {...this.data}>
        <Dom body={this.props.body} api={this.props.api}/>
      </form>
    );
  }
}
