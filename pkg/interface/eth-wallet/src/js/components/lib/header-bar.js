import React, { Component } from 'react';
import classnames from 'classnames';
import { IconHome } from '/components/lib/icons/icon-home';
import { IconSpinner } from '/components/lib/icons/icon-spinner';
import { Sigil } from '/components/lib/icons/sigil';

export class HeaderBar extends Component {
  render() {
    // let spin = (this.props.spinner)
    //   ?  <div className="absolute"
    //        style={{width: 16, height: 16, top: 16, left: 55}}>
    //        <IconSpinner/>
    //      </div>
    //   :  null;

    let popout = (window.location.href.includes("popout/")) 
    ? "dn"
    : "dn db-m db-l db-xl";

    let title = (document.title === "Home")
    ? ""
    : document.title;

    return (
      <div className={"bg-white bg-gray0-d w-100 justify-between relative tc pt3 "
        + popout}
        style={{ height: 40 }}>
        <a className="dib gray2 f9 inter absolute left-1"
          href='/'
          style={{top: 14}}>
          <IconHome/>
          <span className="ml2 white-d v-top lh-title"
          style={{paddingTop: 3}}>
          Home
          </span>
        </a>
        <span className="f9 white-d inter dib"
        style={{
          verticalAlign: "text-top",
          paddingTop: 3
        }}>{title}</span>
        {/* {spin} */}
        <div className="absolute right-1 lh-copy"
        style={{top: 12}}>
        <Sigil
          ship={"~" + window.ship}
          size={16}
          color={"#000000"}
            />
          <span className="mono white-d f9 ml2 v-top">{"~" + window.ship}</span>
        </div>
      </div>
    );
  }
}

