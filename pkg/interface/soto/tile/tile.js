import React, { Component } from 'react';
import classnames from 'classnames';
import _ from 'lodash';


export default class sotoTile extends Component {

  render() {
    return (
      <div className="w-100 h-100 relative" style={{background: "#1a1a1a"}}>
        <a className="w-100 h-100 db no-underline" href="/~soto">
          <p className="gray label-regular b absolute"
           style={{ left: 8, top: 4 }}>
             Dojo
          </p>
          <img src="~soto/img/Tile.png"
            className="absolute"
            style={{ left: 30, top: 30, height: "174px", width: "174px"}}
          />
        </a>
      </div>
    );
  }

}

window.sotoTile = sotoTile;
