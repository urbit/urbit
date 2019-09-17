import React, { Component } from 'react';
import classnames from 'classnames';

export class Widget extends Component {
  constructor(props) {
    super(props);
  }

  componentWillMount() {
    if (this.props.sub) {
      this.props.api.bind(this.props.sub.path,
        "PUT", api.authTokens.ship, this.props.sub.app,
        this.handleEvent.bind(this),
        this.handleError.bind(this));
    }
  }

  handleEvent(diff) {
    this.child.handleEvent(diff);
  }

  handleError(err) {
    console.error(err);
    this.props.api.bind(this.props.path,
      "PUT", api.authTokens.ship, this.props.app,
      this.handleEvent.bind(this),
      this.handleError.bind(this));
  }


  render() {
    let Comp = window.componentTable[this.props.name];
    return (
      <Comp ref={(el) => this.child = el}/>
    );
  }
}
