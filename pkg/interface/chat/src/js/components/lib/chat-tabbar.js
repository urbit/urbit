import React, { Component } from 'react';
import { Route, Link } from "react-router-dom";
import classnames from 'classnames';


export class ChatTabBar extends Component {
  
  render() {
    let props = this.props;

    let memColor = '',
      setColor = '',
      popout = '';

    if (props.location.pathname.includes('/settings')) {
      memColor =  'gray3';
      setColor = 'black white-d';
    } else if (props.location.pathname.includes('/members')) {
      memColor =  'black white-d';
      setColor = 'gray3';
    } else {
      memColor =  'gray3';
      setColor = 'gray3';
    }

    popout = props.location.pathname.includes("/popout")
      ? "popout/" :  "";

    let hidePopoutIcon = (this.props.popout)
      ? "dn-m dn-l dn-xl" : "dib-m dib-l dib-xl";

    return (
      <div className="dib flex-shrink-0 flex-grow-1">
        {!!props.isOwner ? (
          <div className={"dib pt2 f9 pl6 lh-solid"}>
            <Link
              className={"no-underline " + memColor}
              to={`/~chat/` + popout + `members` + props.station}>
              Members
            </Link>
          </div>
        ) : (
          <div className="dib" style={{ width: 0 }}></div>
        )}
        <div className={"dib pt2 f9 pl6 pr6 lh-solid"}>
          <Link
            className={"no-underline " + setColor}
            to={`/~chat/` + popout + `settings` + props.station}>
            Settings
          </Link>
        </div>
        <a href={`/~chat/popout/room` + props.station} target="_blank"
        className="dib fr pt2 pr1">
          <img
            className={`flex-shrink-0 pr3 dn ` + hidePopoutIcon}
            src="/~chat/img/popout.png"
            height="16"
            width="16"/>
        </a>
      </div>
    );
  }
}
