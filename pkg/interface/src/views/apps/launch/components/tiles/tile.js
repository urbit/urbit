import React from 'react';


export default class Tile extends React.Component {
  render() {
    const { transparent } = this.props;
    const bgClasses = transparent ? ' ' : ' bg-transparent ';
    return (
      <div className={"fl mr2 ml2 mb3 mt0 overflow-hidden" + bgClasses}
           style={{ height: '126px', width: '126px' }}>
      {this.props.children}
      </div>
    );
  }
}
