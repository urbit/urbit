import React, { Component } from 'react';

export class Spinner extends Component {
  render() {
    const classes = this.props.classes ? this.props.classes : '';
    const text = this.props.text ? this.props.text : '';
    const awaiting = this.props.awaiting ? this.props.awaiting : false;

    if (awaiting) {
      return (
        <div className={classes + ' z-2 bg-white bg-gray0-d white-d'}>
          <img className="invert-d spin-active v-mid"
            src="/~chat/img/Spinner.png"
            width={16}
            height={16}
          />
          <p className="dib f9 ml2 v-mid inter">{text}</p>
        </div>
      );
    } else {
      return null;
    }
  }
}
