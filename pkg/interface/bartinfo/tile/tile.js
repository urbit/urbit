import React, { Component } from 'react';
import _ from 'lodash';


export default class bartinfoTile extends Component {

  render() {
    return (
      <div className="w-100 h-100 relative bg-white bg-gray0-d ba b--black b--gray1-d">
        <a className="w-100 h-100 db pa2 no-underline" href="/~bartinfo">
          <p className="black white-d absolute f9" style={{ left: 8, top: 8 }}>BART Info</p>
          <img className="absolute" src="/~bartinfo/img/Temp-Bart-Icon.png" style={{top: 40, left: 20, width: "50%"}}/>
        </a>
      </div>
    );
  }

}

window.bartinfoTile = bartinfoTile;
