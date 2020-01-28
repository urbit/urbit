import React, { Component } from 'react';
import { Comments } from './comments';

//TODO ask for note if we don't have it
//TODO initialise note if no state

//TODO if comments are disabled on the notebook, don't render comments
export class Note extends Component {
  render() {
    return (


      <div className="flex justify-center mt4 ph4 pb4">
        <div className="w-100 mw6">
          <div className="flex flex-column">
            <h7 className="f9 mb1">Title</h7>
            <div className="flex">
            <div className="di f9 mono gray2 mr2">Author</div>
            <div className="di f9 gray2">11d ago</div>
            </div>
          </div>

          <p className="f9 mt7 mb12">
            I dreamt of urbit hardware - stars and galaxies and planets had differing forms and size - solid and friendly forms, non-illuminated, state clear, stone-like, -henge-like
          </p>

          <div className="flex mt4">
            <a href="" className="di flex-column w-50 pv6 bt br bb b--gray4">
              <div className="f9 gray2 mb2">Previous</div>
              <div className="f9 mb1">%loud</div>
              <div className="f9 gray2">14d ago</div>
            </span>
            <a href="" className="di flex-column tr w-50 pv6 bt bb b--gray4">
            <div className="f9 gray2 mb2">Next</div>
            <div className="f9 mb1">+advent of %code ~2019.12</div>
            <div className="f9 gray2">6d ago</div>
            </div>
          </div>


        </div>
      </div>
    )
  }
}

export default Note
