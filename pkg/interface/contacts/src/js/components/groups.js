import React, { Component } from 'react';

export class Groups extends Component {
  // drawer to the left
  render() {
    return (
      <div className="br b--gray4 h-100 flex-basis-100-s flex-basis-300-ns">
      <h2 className="f9 pa4 gray2">Your Root Identity</h2>
      <h2 className="f9 pa4 gray2">Your Groups</h2>
      </div>
    )
  }
}

export default Groups;
