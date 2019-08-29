import React, { Component } from 'react';
import classnames from 'classnames';

import { Text }   from '/components/lib/text';
import { Button } from '/components/lib/button';
import { Form } from '/components/lib/form';

export class Dom extends Component {
  constructor(props) {
    super(props);
    this.parseDom = this.parseDom.bind(this);
  }

  parseDom(dom) {
    if (Array.isArray(dom)) {
      return dom.map((itm, i) => {
        return (
          <div key={i}>
            {this.parseDom(itm)}
          </div>
        );
      });
    }

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
      case "form":
        return (
          <Form app={body.app}
            mark={body.mark}
            body={body.body}
            api={this.props.api}/>
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
