import React, { Component } from 'react';
import classnames from 'classnames';

export class TitleSnippet extends Component {
  constructor(props){
    super(props);
  }

  render() {
    return (
      <p className="body-large b two-lines"
        style={{WebkitBoxOrient: "vertical"}}>
        {this.props.title}
      </p>
    );
  }
}

