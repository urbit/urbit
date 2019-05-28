import React, { Component } from 'react';
import classnames from 'classnames';


export class MemberElement extends Component {

  onRemove() {
    const { props } = this;
    props.api.unpermit(props.circle, props.ship);
  }

  render() {
    const { props } = this;

    let actionElem;
    if (props.isHost) {
      actionElem = (
        <p className="dib w-40 underline black label-small-mono label-regular">
          Host
        </p>
      );
    } else {
      actionElem = (
        <a onClick={this.onRemove.bind(this)}
           className="w-40 dib underline black btn-font">
           Remove
        </a>
      );
    }

    return (
      <div>
        <a
          className={
            "w-60 dib underline black pr3 mb2 label-small-mono label-regular"
          }
          href={`/~profile/${props.ship}`}>
          {props.ship}
        </a>
        {actionElem}
      </div>
    );
  }
}


