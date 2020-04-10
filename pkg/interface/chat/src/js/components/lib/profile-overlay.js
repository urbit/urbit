import React, { Component } from "react";
import { Link } from "react-router-dom";
import { cite } from "/lib/util";
import { Sigil } from "/components/lib/icons/sigil";

const HEIGHT = 250;

export class ProfileOverlay extends Component {
  constructor() {
    super();
  }

  render() {
    const { name, ship, color, topSpace, bottomSpace } = this.props;

    let top, bottom;
    if (topSpace < HEIGHT / 2) {
      top = `0px`;
    }
    if (bottomSpace < HEIGHT / 2) {
      bottom = `0px`;
    }
    if (!(top || bottom)) {
      bottom = `-${Math.round(HEIGHT / 2)}px`;
    }
    const containerStyle = { top, bottom, left: "100%" };

    return (
      <div
        onMouseLeave={this.props.onMouseLeave}
        onMouseEnter={this.props.onMouseEnter}
        style={containerStyle}
        className="flex-col shadow-6 br2 bg-white bg-gray0-d inter absolute z-1 f9 lh-solid"
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
          {name && <div className="b white-d">{name}</div>}
          <div className="mono gray2">{cite(`~${ship}`)}</div>
          <Link
            to={`/~chat/new/dm/~${ship}`}
            className="b--green0 b--green2-d b--solid bw1 green2 mt3 tc pa2 pointer db"
          >
            Send Message
          </Link>
        </div>
      </div>
    );
  }
}
