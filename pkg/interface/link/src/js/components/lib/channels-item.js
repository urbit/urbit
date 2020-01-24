import React, { Component } from 'react'

import { Route, Link } from 'react-router-dom'; 

export class ChannelsItem extends Component {
  render() {
    const { props } = this;

    let selectedClass = (props.selected) 
    ? "bg-gray5 bg-gray1-d b--gray4 b--gray2-d" 
    : "b--transparent";
    
    let memberCount = Object.keys(props.members).length;

    return (
      <Link to={"/~link" + props.link}>
        <div className={"w-100 v-mid f9 pl4 bt bb  " + selectedClass}>
          <p className="f9 pt1">{props.name}</p>
          <p className="f9 gray2">
            {memberCount + " contributor" + ((memberCount === 1) ? "" : "s")}
          </p>
          <p className="f9 pb1">
            {props.linkCount + " link" + ((props.linkCount === 1) ? "" : "s")}
          </p>
        </div>
      </Link>
    );
  }
}

