import React, { Component } from 'react';
import classnames from 'classnames';

import { Text }   from '/components/dom/text';
import { Button } from '/components/dom/button';

export class Dom extends Component {
  constructor(props) {
    super(props);
  }

  parseDom(dom) {
    let head = Object.keys(dom)[0];
    let body = Object.values(dom)[0];
    switch (head) {
      case "text":
        return (
          <Text body={body}/>
        );
      case "button":
        return (
          <Button body={body.body} action={body.action} api={this.props.api}/>
        );
      default:
        return;
    }
  }

  render() {
    return (
      <div className="w-100 h-100 overflow-y-scroll overflow-x-scroll">
        {this.parseDom(this.props.body)}
      </div>
    );
  }
}
