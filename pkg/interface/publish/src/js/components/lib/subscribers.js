import React, { Component } from 'react';
import { Dropdown } from './dropdown';
import { cite } from '../../lib/util';

export class Subscribers extends Component {
  constructor(props){
    super(props);
    this.redirect = this.redirect.bind(this);
    this.addUser = this.addUser.bind(this);
    this.removeUser = this.removeUser.bind(this);
  }

  addUser(who, path) {
    let action = {
      add: {
        members: [who],
        path: path,
      }
    }
    window.api.action("group-store", "group-action", action);
  }

  removeUser(who, path) {
    let action = {
      remove: {
        members: [who],
        path: path,
      }
    }
    window.api.action("group-store", "group-action", action);
  }

  redirect(url) {
    window.location.href = url;
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
        let width = 0;
        let options = [];
        if (readPath === writePath) {
          width = 258;
          let url = `/~contacts${writePath}`;
          options = [{
            cls: "bg-transparent white-d tl pointer w-100 db hover-bg-gray4 hover-bg-gray1-d ph2 pv3",
            txt: "Manage this group in the contacts view",
            action: () => {this.redirect(url)}
          }];
        } else {
          width = 157;
          options = [{
            cls: "bg-transparent white-d tl pointer w-100 db hover-bg-gray4 hover-bg-gray1-d ph2 pv3",
            txt: "Demote to subscriber",
            action: () => {this.removeUser(`~${who}`, writePath)}
          }];
        }
        return (
          <div className="flex justify-between" key={i}>
            <div className="f9 mono mr2">{`${cite(who)}`}</div>
            <Dropdown
              options={options}
              width={width}
              buttonText={"Options"}
            />
          </div>
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
      if (this.props.notebook.subscribers){
        let width = 162;
        subscribers = this.props.notebook.subscribers.map((who, i) => {
          let options = [
            { cls: "white-d tl pointer w-100 db hover-bg-gray4 hover-bg-gray1-d bg-transparent ph2 pv3",
              txt: "Promote to participant",
              action: () => {this.addUser(who, writePath)}
            },
            { cls: "tl red2 pointer w-100 db hover-bg-gray4 hover-bg-gray1-d bg-transparent ph2 pv3",
              txt: "Ban",
              action: () => {this.addUser(who, readPath)}
            },
          ];
          return (
            <div className="flex justify-between" key={i}>
              <div className="f9 mono mr2">{cite(who)}</div>
              <Dropdown
                options={options}
                width={width}
                buttonText={"Options"}
              />
            </div>
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
      let width = 72;
      let banned = Array.from(readPerms.who).map((who, i) => {
        let options = [{
          cls: "tl red2 pointer",
          txt: "Unban",
          action: () => {this.removeUser(`~${who}`, readPath)}
        }];
        return (
          <div className="flex justify-between" key={i}>
            <div className="f9 mono mr2">{`~${who}`}</div>
            <Dropdown
              options={options}
              width={width}
              buttonText={"Options"}
            />
          </div>
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
            <div className="f9 mono mr2">{cite(this.props.host)}</div>
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
