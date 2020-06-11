import React, { Component } from 'react';
import { Sigil } from './sigil';
import { GroupFilter } from './group-filter';
import _ from 'lodash';

export default class Header extends Component {
  render() {

    let invites = (this.props.invites && this.props.invites["/contacts"])
      ? this.props.invites["/contacts"] : {};

    return (
      <header
        className={"bg-white bg-gray0-d w-100 justify-between relative " +
        "tl pt3"}
        style={{ height: 37 }}>
        <div className="fl lh-copy absolute left-1" style={{top: 9}}>
          <a href="/~groups/me"><Sigil
            ship={"~" + window.ship}
            size={16} color={"#000000"}
            classes="mix-blend-diff v-mid" />
          </a>
          <GroupFilter invites={invites} associations={this.props.associations}/>
        <span
          className="f9 white-d inter dib ml1 c-default">
          <span className="inter gray2 f9 dib mr2">/</span> Home
        </span>
        </div>
      </header>
    );
  }

}

