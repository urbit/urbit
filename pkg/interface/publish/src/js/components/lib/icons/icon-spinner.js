import React, { Component } from 'react';

export class Spinner extends Component {
  render() {

    let classes = !!this.props.classes ? this.props.classes : "";
    let text = !!this.props.text ? this.props.text : "";
    let awaiting = !!this.props.awaiting ? this.props.awaiting : false;

    if (awaiting) {
      return (
        <div className={classes + " z-2 bg-white bg-gray0-d white-d"}>
        <img className="invert-d spin-active v-mid"
          src="/~publish/Spinner.png"
          width={16}
          height={16} />
          <p className="dib f9 ml2 v-mid">{text}</p>
        </div>
      );
    }
    else {
      return null;
    }
  }
}
