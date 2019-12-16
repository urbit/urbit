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
      setColor = 'black';
    } else if (props.location.pathname.includes('/members')) {
      memColor =  'black';
      setColor = 'gray3';
    } else {
      memColor =  'gray3';
      setColor = 'gray3';
    }

    (props.location.pathname.includes('/popout')) 
    ? popout = "popout/"
    : popout = "";

    let hidePopoutIcon = (this.props.popout) 
    ? "dn-m dn-l dn-xl" 
    : "dib-m dib-l dib-xl";


    return (
      <div className="dib pt2 flex-shrink-0 flex-grow-1">
        {!!props.isOwner ? (
          <div className={"dib f8 pl6"}>
            <Link
              className={"no-underline " + memColor}
              to={`/~chat/` + popout + `members` + props.station}>
              Members
            </Link>
          </div>
        ) : (
          <div className="dib" style={{ width: 0 }}></div>
        )}
        <div className={"dib f8 pl6 pr6"}>
          <Link
            className={"no-underline " + setColor}
            to={`/~chat/` + popout + `settings` + props.station}>
            Settings
          </Link>
        </div>
        <a href={`/~chat/popout/room` + props.station} target="_blank"
        className="dib fr">
          <img
            className={`flex-shrink-0 pr2 dn ` + hidePopoutIcon}
            src="/~chat/img/popout.png"
            height="16"
            width="16"/>
        </a>
      </div>
    );
  }
}
