import React, { Component } from 'react';
import classnames from 'classnames';

import { TextInput } from '/components/lib/text-input';
import { Submit }    from '/components/lib/submit';

export class FormDom extends Component {
  constructor(props) {
    super(props);
    this.parseDom = this.parseDom.bind(this);
  }

  parseDom(dom, key) {
    if (Array.isArray(dom)) {
      return dom.map((itm, i) => {
        return this.parseDom(itm, i)
      });
    }
    let head = Object.keys(dom)[0];
    let body = Object.values(dom)[0];
    switch (head) {
      case "text-input":
        return (
          <TextInput name={body.name} key={key}/>
        );
      case "submit":
        return (
          <Submit body={body} action={this.props.action} key={key}/>
        );
      default:
        return;
    }
  }

  render() {
    return this.parseDom(this.props.body, 0);
  }
}
