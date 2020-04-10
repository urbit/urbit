import React, { Component } from "react";
import { Sigil } from "/components/lib/icons/sigil";

const HEIGHT = 250;

export class ProfileOverlay extends Component {
  constructor() {
    super();
  }

  render() {
    const { name, ship, color, offset, height } = this.props;

    let top, bottom;
    if (offset < HEIGHT / 2) {
      top = `0px`;
    }
    if (height - offset < HEIGHT / 1.5) {
      bottom = `0px`;
    }
    if (!(top || bottom)) {
      bottom = `-${Math.round(HEIGHT / 2) - 20}px`;
    }
    const containerStyle = { top, bottom, left: "100%" };

    return (
      <div
        onMouseLeave={this.props.onMouseLeave}
        onMouseEnter={this.props.onMouseEnter}
        style={containerStyle}
        className="flex-col shadow-6 br2 bg-white inter absolute z-1 f9 lh-solid"
      >
        <div style={{ height: "160px" }}>
          <Sigil
            ship={ship}
            size={160}
            color={color}
            classes="brt2"
            svgClass="brt2"
          />
        </div>
        <div className="pv3 pl3 pr2">
          {name && <div className="b">{name}</div>}
          <div className="mono gray2">{`~${ship}`}</div>
          <div className="b--green0 b--solid bw1 green2 mt3 tc pa2 pointer">
            Send Message
          </div>
        </div>
      </div>
    );
  }
}
