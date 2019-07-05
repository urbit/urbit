import React, { Component } from 'react';
import classnames from 'classnames';

export class TitleSnippet extends Component {
  constructor(props){
    super(props);
  }

  render() {
    return (
      <p className="body-large b title-preview"
        style={{webkitBoxOrient: "vertical"}}>
        {this.props.title}
      </p>
    );
  }
}

