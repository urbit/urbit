import React, { Component } from 'react';
import { SubscriberItem } from './subscriber-item';

export class Subscribers extends Component {
  constructor(props){
    super(props);
  }

  render() {
    let readPath = this.props.notebook["subscribers-group-path"]
    let readPerms = (readPath)
      ? this.props.permissions[readPath]
      : null;
    let writePath = this.props.notebook["writers-group-path"]
    let writePerms = (writePath)
      ? this.props.permissions[writePath]
      : null;

    let writers = [];
    if (writePerms && writePerms.kind === 'white') {
      let withoutUs = new Set(writePerms.who)
      withoutUs.delete(window.ship);
      writers = Array.from(withoutUs).map((who, i) => {
        return (
          <SubscriberItem key={i}
            readPath={readPath}
            writePath={writePath}
            who={`~${who}`}
            readPerms={readPerms}
            writePerms={writePerms}
            section='participants'
          />
        )
      });
    }

    if (writers.length === 0) {
      writers =
        <div className="f9">
          There are no participants on this notebook.
        </div>
    }

    let subscribers = null;
    if (readPath !== writePath) {
      if (readPerms && readPerms.kind === 'white') {
        let withoutUs = new Set(readPerms.who)
        withoutUs.delete(window.ship);
        subscribers = Array.from(withoutUs).map((who, i) => {
          return (
            <SubscriberItem key={i}
              readPath={readPath}
              writePath={writePath}
              who={`~${who}`}
              readPerms={readPerms}
              writePerms={writePerms}
              section='subscribers'
            />
          )
        });
      } else if (this.props.notebook.subscribers){
        subscribers = this.props.notebook.subscribers.map((who, i) => {
          return (
            <SubscriberItem key={i}
              readPath={readPath}
              writePath={writePath}
              who={who}
              readPerms={readPerms}
              writePerms={writePerms}
              section='subscribers'
            />
          )
        });
      }
      if (subscribers.length === 0) {
        subscribers =
          <div className="f9">
            There are no subscribers to this notebook.
          </div>
      }
    }

    let subsContainer = (readPath === writePath)
      ?  null
      :  <div className="flex flex-column">
           <div className="f9 gray2 mt6 mb3">Subscribers (read access only)</div>
           {subscribers}
         </div>;


    let bannedContainer = null;
    if (readPerms && readPerms.kind === 'black') {
      let banned = Array.from(readPerms.who).map((who, i) => {
        return (
          <SubscriberItem key={i}
            readPath={readPath}
            writePath={writePath}
            who={`~${who}`}
            readPerms={readPerms}
            writePerms={writePerms}
            section='banned'
          />
        )
      });
      if (banned.length === 0) {
        banned =
          <div className="f9">
            There are no users banned from this notebook.
          </div>
      }
      bannedContainer =
        <div className="flex flex-column">
          <div className="f9 gray2 mt6 mb3">Banned</div>
          {banned}
        </div>;
    }


    return (
      <div>
        <div className="flex flex-column">
          <div className="f9 gray2">Host</div>
          <div className="flex justify-between mt3">
            <div className="f9 mono mr2">{this.props.host}</div>
          </div>
        </div>
        <div className="flex flex-column">
          <div className="f9 gray2 mt6 mb3">
            Participants (read and write access)
          </div>
          {writers}
        </div>
        {subsContainer}
        {bannedContainer}
      </div>
    )
  }
}

export default Subscribers
