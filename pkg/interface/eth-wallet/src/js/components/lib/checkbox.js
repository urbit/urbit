import React, { Component } from 'react';

export class Checkbox extends Component {
  render() {
    const { label, isActive, toggle } = this.props;
    return (
      <div key={label}
           className="flex items-center mb2 pointer lh-copy"
           onClick={() => toggle()}>
        <div className="flex mr3 f8 lh-tall us-none pointer flex-row align-center">
          <div className={`flex align-center justify-center p1 mr3 white b--gray4 b--gray1-d ba
               ${isActive ? ' bg-black' : ' bg-white'}`}
               style={{
                 height: '24px',
                 width: '24px'
               }}>
            {isActive && '✓'}
          </div>
          {label}
        </div>
      </div>);
  }
}
