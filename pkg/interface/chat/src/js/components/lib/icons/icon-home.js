import React, { Component } from "react";

export class IconHome extends Component {
  render() {
    return (
      //TODO relocate to ~launch when OS1 is ported
      <img
        className="invert-d"
        src="/~chat/img/Home.png"
        width={16}
        height={16}
      />
    );
  }
}
