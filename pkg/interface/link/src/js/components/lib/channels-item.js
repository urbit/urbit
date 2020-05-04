import React, { Component } from 'react'
import { Route, Link } from 'react-router-dom';
import { makeRoutePath } from '../../lib/util';

export class ChannelsItem extends Component {
  render() {
    const { props } = this;

    let selectedClass = (props.selected)
    ? "bg-gray5 bg-gray1-d"
    : "pointer hover-bg-gray5 hover-bg-gray1-d";

    const unseenCount = props.unseenCount > 0
      ? <span className="dib white bg-gray3 bg-gray2-d fw6 br1" style={{padding: "1px 5px"}}>{props.unseenCount}</span>
      : null;

    return (
      <Link to={makeRoutePath(props.link)}>
        <div className={"w-100 v-mid f9 ph4 z1 pv1 " + selectedClass}>
          <p className="f9 dib">{props.name}</p>
          <p className="f9 dib fr">
            {unseenCount}
          </p>
        </div>
      </Link>
    );
  }
}

