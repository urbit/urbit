import React, { Component } from 'react';
import { subscription } from '/subscription';
import { api } from '/lib/api';
import classnames from 'classnames';

export default class Tile extends Component {

  constructor(props) {
    super(props);
  }

  render() {
    let SpecificTile = window[this.props.type + 'Tile'];

    return (
      <div className="fl ma2 bg-white bg-gray0-d overflow-hidden"
           style={{ height: '126px', width: '126px' }}>
        { !!SpecificTile ?
          <SpecificTile data={this.props.data} />
          : <div></div>
        }
      </div>
    );
  }

}

