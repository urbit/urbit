import React from 'react';


export default class Tile extends React.Component {
  render() {
    const { transparent } = this.props;
    const bgClasses = transparent ? ' ' : ' bg-white bg-gray0-d ';
    return (
      <div className={"fl ma2 overflow-hidden" + bgClasses}
           style={{ height: '126px', width: '126px' }}> 
      {this.props.children} 
      </div>
    );
  }
}
