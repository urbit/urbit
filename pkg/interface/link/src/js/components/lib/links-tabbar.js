import React, { Component } from 'react';
import { Link } from 'react-router-dom';
import { makeRoutePath } from '../../lib/util';

export class LinksTabBar extends Component {
  render() {
    let props = this.props;

    let memColor = '',
        setColor = '';

    if (props.location.pathname.includes('/settings')) {
      memColor = 'gray3';
      setColor = 'black white-d';
    } else if (props.location.pathname.includes('/members')) {
      memColor = 'black white-d';
      setColor = 'gray3';
    } else {
      memColor = 'gray3';
      setColor = 'gray3';
    }

    let hidePopoutIcon = (props.popout)
    ? "dn-m dn-l dn-xl"
    : "dib-m dib-l dib-xl";

    return (
      <div className="dib flex-shrink-0 flex-grow-1">
        {!!props.amOwner ? (
          <div className={"dib pt2 f9 pl6 lh-solid"}>
            <Link
              className={"no-underline " + memColor}
              to={makeRoutePath(props.resourcePath, props.popout) + '/members'}>
              Members
            </Link>
          </div>
        ) : (
          <div className="dib" style={{ width: 0 }}></div>
        )}
        <div className={"dib pt2 f9 pl6 pr6 lh-solid"}>
          <Link
            className={"no-underline " + setColor}
            to={makeRoutePath(props.resourcePath, props.popout) + '/settings'}>
            Settings
          </Link>
        </div>
        <a href={makeRoutePath(props.resourcePath, true, props.page)}
          target="_blank"
          className="dib fr pt2 pr1">
          <img
            className={`flex-shrink-0 pr3 dn ` + hidePopoutIcon}
            src="/~link/img/popout.png"
            height="16"
            width="16"/>
        </a>
      </div>
    );
  }
}

export default LinksTabBar
