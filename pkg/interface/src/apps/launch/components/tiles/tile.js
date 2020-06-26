import React from 'react';


export default class Tile extends React.Component {
  render() {
    return (
      <div className="fl ma2 bg-white bg-gray0-d overflow-hidden"
           style={{ height: '126px', width: '126px' }}>
        {this.props.children}
      </div>
    );
  }
}
