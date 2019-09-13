import React, { Component } from 'react';
import classnames from 'classnames';
import { Dom } from '/components/dom';

export class Button extends Component {
  constructor(props) {
    super(props);
    this.buttonAction = this.buttonAction.bind(this);
  }

  buttonAction(evt){
    this.props.api.action(this.props.action.app,
      this.props.action.mark, this.props.action.dat);
  }

  render() {
    return (
      <div>
        <button onClick={this.buttonAction}>
          <Dom body={this.props.body}/>
        </button>
      </div>
    );
  }
}
