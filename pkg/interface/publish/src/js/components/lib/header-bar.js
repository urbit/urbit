import React, { Component } from "react";
import { GroupFilter } from "./group-filter";
import { Sigil } from "/components/lib/icons/sigil";

export class HeaderBar extends Component {
  render() {
    let popout = window.location.href.includes("popout/")
      ? "dn" : "dn db-m db-l db-xl";

    let invites = (this.props.invites && this.props.invites["/contacts"])
      ? this.props.invites["/contacts"]
      : {};

    return (
      <div
        className={
          "bg-white bg-gray0-d w-100 justify-between relative tc pt3 " + popout
        }
        style={{ height: 45 }}>
        <div className="fl lh-copy absolute left-0" style={{ top: 8 }}>
          <a href="/~groups/me" className="dib v-mid">
            <Sigil
              ship={"~" + window.ship}
              classes="v-mid mix-blend-diff"
              size={16}
              color={"#000000"}
            />
          </a>
          <GroupFilter invites={invites} associations={this.props.associations} />
          <span className="dib f9 v-mid gray2 ml1 mr1 c-default inter">/</span>
          <a
            className="dib f9 v-mid inter ml2"
            href="/"
            style={{ top: 14 }}>
            ⟵</a> <p className="dib f9 v-mid inter ml2 white-d">Publishing</p>
        </div>
      </div>
    );
  }
}
