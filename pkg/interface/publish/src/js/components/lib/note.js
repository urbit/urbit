import React, { Component } from 'react';
import { Comments } from './comments';

//TODO ask for note if we don't have it
//TODO initialise note if no state

//TODO if comments are disabled on the notebook, don't render comments
export class Note extends Component {
  render() {
    return (
      <div>
      <div className="f7 sans">Title</div>
      <div className="f7 mono gray-60">Author</div>
      <Body/>
      <Previous className="pv10 bt br bb b-gray">
        <h7>Previous Post</h7>
        <h7>Title</h7>
        <h7 className="gray">Date</h7>
      </Previous>
      <Next className="pv10 bt bl bb b-gray">
      <h7>Next Post</h7>
      <h7>Title</h7>
      <h7 className="gray">Date</h7>
      </Next>


      <Comments/>
      </div>
    )
  }
}

export default Note
