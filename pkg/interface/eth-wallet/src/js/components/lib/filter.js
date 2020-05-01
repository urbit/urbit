import React, { Component } from 'react';

export class Filter extends Component {
  render() {
    const { label, isActive, onClick } = this.props;
    return (<a style={{height:'30px', whiteSpace: 'nowrap'}}
               className={`f9 link br-pill ba ph3 pv2 ma2 near-black ${isActive ? 'o-30': ''}`}
               href="#" onClick={onClick}>{label}</a>);
  }
}
