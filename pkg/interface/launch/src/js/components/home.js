import React, { Component } from 'react';
import { subscription } from '/subscription';
import { api } from '/lib/api';
import classnames from 'classnames';

import Header from '/components/header';
import Tile from '/components/tile';


export default class Home extends Component {

  constructor(props) {
    super(props);
    subscription.subscribe("/main");
  }

  render() {
    let keys = [...this.props.keys];
    let tileElems = keys.map((tile) => {
      return (
        <Tile key={tile} type={tile} data={this.props.data[tile]} />
      );
    });

    return (
      <div className="fl w-100 h-100 bg-white center">
        <Header />
        <div className={"v-mid pa2 dtc-m dtc-l dtc-xl " +
        "flex justify-between flex-wrap"}
        style={{maxWidth: "40rem"}}>
          {tileElems}
        </div>
      </div>
    );
  }

}

