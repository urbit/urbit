import React, { Component } from 'react';
import { Comments } from './comments';

//TODO ask for note if we don't have it
//TODO initialise note if no state

//TODO if comments are disabled on the notebook, don't render comments
export class Note extends Component {
  render() {
    return (
      <div>
        <Comments/>
      </div>
    )
  }
}

export default Note
