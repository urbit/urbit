import React, { Component } from 'react';
import { subscription } from '/subscription';
import { api } from '/lib/api';
import classnames from 'classnames';

import Header from '/components/header';
import Tile from '/components/tile';

const loadExternalScript = (ext, callback) => {
  const script = document.createElement('script');
  script.src = '/~' + ext + '/tile.js';
  script.id = ext;
  document.body.appendChild(script);

  script.onload = () => {
    console.log('callback');
    if (callback) callback();
  };
};

export default class Home extends Component {

  constructor(props) {
    super(props);

    this.loadedScripts = new Set();
    subscription.subscribe("/main");
  }

  componentDidUpdate(prevProps, prevState) {
    let difference = new Set(
      [...this.props.keys]
        .filter(x => !prevProps.keys.has(x))
    );

    difference.forEach((external) => {
      loadExternalScript(external, this.forceUpdate.bind(this));
    });
  }

  render() {
    let keys = [...this.props.keys];
    let tileElems = keys.map((tile) => {
      return (
        <Tile key={tile} type={tile} data={this.props.data[tile]} />
      );
    });
    
    return (
      <div className="fl w-100 vh-100 bg-black center">
        <Header />
        <div className="v-mid pa2 dtc">
          {tileElems}
        </div>
      </div>
    );
  }

}

