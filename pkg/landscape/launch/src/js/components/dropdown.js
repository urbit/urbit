import React, { Component } from 'react';
import { subscription } from '/subscription';
import { api } from '/lib/api';
import classnames from 'classnames';

let style = {
  circle: {
    width: '2em',
    height: '2em',
    background: '#000000',
    border: '4px solid #333333',
    'borderRadius': '2em'
  },
  triangle: {
    width: '0px',
    height: '0px',
    'borderTop': '8px solid #FFFFFF',
    'borderLeft': '8px solid transparent',
    'borderRight': '8px solid transparent',
    'fontSize': 0,
    'lineHeight': 0,
    'marginLeft': 'auto',
    'marginRight': 'auto',
  }
};


export default class Dropdown extends Component {

  constructor(props) {
    super(props);
  }

  render() {
    return (
      <div>
        <div className="ml2" style={style.circle}>
            <div className="mt2" style={style.triangle}></div>
        </div>
      </div>
    );
  }

}


