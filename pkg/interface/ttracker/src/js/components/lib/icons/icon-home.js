import React, { Component } from "react";

export class IconHome extends Component {
  render() {
    let classes = !!this.props.classes ? this.props.classes : "";
    return (
      <img
        className={"invert-d " + classes}
        src="/~ttracker/img/Home.png"
        width={16}
        height={16}
      />
    );
  }
}
