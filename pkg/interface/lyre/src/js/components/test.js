import React, { Component } from 'react';
import classnames from 'classnames';

export class TestComp extends Component {
  render() {
    return (
      <div className="w-100 h-100 ba b--red flex">
        <div className="h-100 ba b--green flex-column flex" style={{flexBasis: 320}}>
            <div className="bg-silver w-100" style={{flexBasis:88}}> 
            </div>
            <div className="bg-light-silver w-100" style={{flexGrow:1}}>
            </div>
        </div>
        <div className="h-100 ba b--blue flex-column flex" style={{flexGrow: 1}}>
          <div className="bg-green w-100" style={{flexBasis:88}}> 
          </div>
          <div className="bg-red w-100" style={{flexGrow:1}}>
          </div>
          <div className="bg-blue w-100" style={{flexBasis:188}}>
          </div>
        </div>
      </div>
    );
  }
}
