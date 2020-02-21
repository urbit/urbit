import React, { Component } from "react";
import { IconHome } from "/components/lib/icons/icon-home";
import { Sigil } from "/components/lib/icons/sigil";

export class HeaderBar extends Component {
  constructor(props) {
    super(props);
  }

  render() {
    let popout = window.location.href.includes("popout/")
      ? "dn"
      : "dn db-m db-l db-xl";

    let title = document.title === "Home" ? "" : document.title;

    let spinner = !!this.props.spinner ? this.props.spinner : false;

    let spinnerClasses = "";

    if (spinner === true) {
      spinnerClasses = "spin-active";
    }

    return (
      <div
        className={"bg-white bg-gray0-d w-100 justify-between relative tc pt3 " + popout}
        style={{ height: 40 }}>
        <a
          className="dib gray2 f9 inter absolute left-0 white-d"
          href="/"
          style={{ top: 14 }}>
          <IconHome classes={spinnerClasses} />
          <span className="ml2 v-top lh-title" style={{ paddingTop: 3 }}>
            Home
          </span>
        </a>
        <span
          className="f9 inter dib white-d"
          style={{
            verticalAlign: "text-top",
            paddingTop: 3
          }}>
          {title}
        </span>
        <div className="absolute right-0 lh-copy" style={{ top: 9 }}>
          <Sigil
            ship={"~" + window.ship}
            classes="mix-blend-diff v-mid"
            size={16}
            color={"#000000"}
          />
          <span className="mono f9 ml2 white-d">{"~" + window.ship}</span>
        </div>
      </div>
    );
  }
}
