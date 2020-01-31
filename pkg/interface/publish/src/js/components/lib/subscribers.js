import React, { Component } from 'react';
import { SubscriberItem } from './subscriber-item';

//TODO map list of subscriber-items from props

export class Subscribers extends Component {
  render() {
    return (
      <div>
      <div className="flex flex-column">
        <div className="f9 gray2">Host</div>
        <div className="flex justify-between mt3">
          <div className="flex">
            <div className="f9 mono mr2">~fabled-faster</div>
            <div className="f9 gray2">Last active</div>
          </div>
          <div className="f9">Options ⌃</div>
        </div>
      </div>
      <div className="flex flex-column">
        <div className="f9 gray2 mt6">Participants (read and write access)</div>
        <div className="f9 mt3">There are no paticipants in this notebook.</div>
        <div className="flex justify-between mt3">
          <div className="flex">
            <div className="f9 mono mr2">~fabled-faster</div>
            <div className="f9 gray2">Last active</div>
          </div>
          <div className="f9">Options ⌃</div>
        </div>
      </div>
      <div className="flex flex-column">
        <div className="f9 gray2 mt6 mb3">Subscribers (read access only)</div>
        <div className="flex justify-between">
          <div className="flex">
            <div className="f9 mono mr2">~fabled-faster</div>
            <div className="f9 gray2">Last active</div>
          </div>
          <div className="f9">Options ⌃</div>
        </div>
      </div>
      <div className="flex flex-column">
        <div className="f9 gray2 mt6 mb3">Banned</div>
        <div className="flex justify-between">
          <div className="flex">
            <div className="f9 mono mr2">~fabled-faster</div>
            <div className="f9 gray2">Last active</div>
          </div>
          <div className="f9">Options ⌃</div>
        </div>
      </div>
      </div>
    )
  }
}

export default Subscribers
