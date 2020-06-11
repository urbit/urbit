import React, { Component } from 'react';
import { subscription } from '/subscription';
import { api } from '/lib/api';
import classnames from 'classnames';

import Header from '/components/header';
import Tile from '/components/tile';
import Welcome from '/components/welcome';

export default class Home extends Component {

  constructor(props) {
    super(props);
    subscription.subscribe("/main");
  }

  render() {
    let keys = [...this.props.keys];
    let tileElems = keys.map((tile) => {
      let tileData = {};
      if (tile in this.props.data && tile !== "invites") {
        tileData = this.props.data[tile] !== null
          ? this.props.data[tile] : {};

        // tileData["invites"] = ("invites" in this.props.data)
        // ? this.props.data["invites"] : {};
      }
      if ((tile !== "invites") && (tile !== "associations")) {
        return <Tile key={tile} type={tile} data={tileData} />;
      }
    });

    let headerData = {};
    if ("invites" in this.props.data) {
      headerData["invites"] = this.props.data["invites"];
    }

    return (
      <div className="fl w-100 h-100 bg-white bg-gray0-d center">
        <Header data={headerData} associations={this.props.data.associations} invites={this.props.data.invites}/>
        <div className={"v-mid ph2 dtc-m dtc-l dtc-xl " +
        "flex justify-between flex-wrap"}
        style={{maxWidth: "40rem"}}>
          <Welcome />
          {tileElems}
        </div>
      </div>
    );
  }

}

