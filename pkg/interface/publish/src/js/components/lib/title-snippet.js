import React, { Component } from 'react';
import classnames from 'classnames';

export class TitleSnippet extends Component {
  constructor(props){
    super(props);
  }

  render() {
    if (this.props.badge) {
      return (
        <div className="body-large two-lines b"
            style={{WebkitBoxOrient: "vertical"}}>
          <span className="h2 green-medium"> • </span>
          <span>
            {this.props.title}
          </span>
        </div>
      );
    } else {
      return (
        <p className="body-large b two-lines"
          style={{WebkitBoxOrient: "vertical"}}>
          {this.props.title}
        </p>
      );
    }
  }
}

