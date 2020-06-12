import React, { Component } from "react";
import { Sigil } from "/components/lib/icons/sigil";

export class HeaderBar extends Component {
  render() {
    let popout = window.location.href.includes("popout/")
      ? "dn" : "dn db-m db-l db-xl";

    let invites = (this.props.invites && this.props.invites.contacts)
      ? this.props.invites.contacts
      : {};

    return (
      <div
        className={
          "bg-white bg-gray1-d w-100 justify-between relative tc pt3 " + popout
        }
        style={{ height: 45 }}>
        <div className="fl lh-copy absolute left-1" style={{ top: 8 }}>
          <a href="/~groups/me" className="dib v-top inter">
            <Sigil
              ship={"~" + window.ship}
              classes="v-mid mix-blend-diff"
              size={16}
              color={"#000000"}
            />
          </a>
          <a
            className="dib f9 v-mid inter ml2 black white-d"
            href="/"
            style={{ top: 14 }}>
            ⟵</a> <p className="dib f9 v-mid inter ml2 white-d">Dojo</p>
        </div>
      </div>
    );
  }
}
