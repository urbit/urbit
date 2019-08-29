import React, { Component } from 'react';
import classnames from 'classnames';
import { Dom } from '/components/dom';

export class Horizontal extends Component {
  constructor(props) {
    super(props);
  }

  render() {
    let list = this.props.body.map((itm, i) => {
      return (<Dom body={itm} key={i} api={this.props.api}/>);
    });
    return (
      <div className="flex">
        {list}
      </div>
    );
  }
}
