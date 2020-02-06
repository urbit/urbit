import React, { Component } from 'react';
import { Sigil } from './sigil';

export default class Header extends Component {
  render() {
    return (
      <header
        className="bg-white bg-gray0-d w-100 justify-between relative tc pt3"
        style={{ height: 40 }}>
        <span
          className="f9 white-d inter dib"
          style={{
            verticalAlign: "text-top",
            paddingTop: 3
          }}>
          Home
        </span>
        <div className="absolute right-1 lh-copy" style={{ top: 12 }}>
          <Sigil
            ship={"~" + window.ship}
            size={16} color={"#000000"}
            classes="mix-blend-diff v-mid" />
          <span className="mono white-d f9 ml2">
            {"~" + window.ship}
          </span>
        </div>
      </header>
    );
  }

}

