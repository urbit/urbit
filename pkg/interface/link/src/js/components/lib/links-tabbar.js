import React, { Component } from 'react'

export class LinksTabBar extends Component {
  render() {
    let props = this.props;

    let memColor = '',
      popout = '';

    if (props.location.pathname.includes('/members')) {
      memColor =  'black';
    } else {
      memColor =  'gray3';
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
              to={`/~link/` + popout + `members` + props.path}>
              Members
            </Link>
          </div>
        ) : (
          <div className="dib" style={{ width: 0 }}></div>
        )}
        <a href={`/~link/popout` + props.path} target="_blank"
        className="dib fr">
          <img
            className={`flex-shrink-0 pr4 dn invert-d ` + hidePopoutIcon}
            src="/~link/img/popout.png"
            height="16"
            width="16"/>
        </a>
      </div>
    );
  }
}

export default LinksTabBar
