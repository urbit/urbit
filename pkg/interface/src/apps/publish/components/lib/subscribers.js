import React, { Component } from 'react';
import { Dropdown } from './dropdown';
import { cite } from '../../../../lib/util';

export class Subscribers extends Component {
  constructor(props) {
    super(props);
    this.redirect = this.redirect.bind(this);
    this.addUser = this.addUser.bind(this);
    this.removeUser = this.removeUser.bind(this);
  }

  addUser(who, path) {
    this.props.api.groups.add(path, [who]);
  }

  removeUser(who, path) {
    this.props.api.groups.remove(path, [who]);
  }

  redirect(url) {
    window.location.href = url;
  }

  render() {
    const readPath = this.props.notebook['subscribers-group-path'];
    const readPerms = (readPath)
      ? this.props.permissions[readPath]
      : null;
    const writePath = this.props.notebook['writers-group-path'];
    const writePerms = (writePath)
      ? this.props.permissions[writePath]
      : null;

    let writers = [];
    if (writePerms && writePerms.kind === 'white') {
      const withoutUs = new Set(writePerms.who);
      withoutUs.delete(window.ship);
      writers = Array.from(withoutUs).map((who, i) => {
        let width = 0;
        let options = [];
        if (readPath === writePath) {
          width = 258;
          const url = `/~groups${writePath}`;
          options = [{
            cls: 'bg-transparent white-d tl pointer w-100 db hover-bg-gray4 hover-bg-gray1-d ph2 pv3',
            txt: 'Manage this group',
            action: () => {
            this.redirect(url);
            }
          }];
        } else {
          width = 157;
          options = [{
            cls: 'bg-transparent white-d tl pointer w-100 db hover-bg-gray4 hover-bg-gray1-d ph2 pv3',
            txt: 'Demote to subscriber',
            action: () => {
            this.removeUser(`~${who}`, writePath);
            }
          }];
        }
        return (
          <div className="flex justify-between" key={i}>
            <div className="f9 mono mr2">{`${cite(who)}`}</div>
            <Dropdown
              options={options}
              width={width}
              buttonText={'Options'}
            />
          </div>
        );
      });
    }

    if (writers.length === 0) {
      writers =
        <div className="f9">
          There are no participants on this notebook.
        </div>;
    }

    let subscribers = null;
    if (readPath !== writePath) {
      if (this.props.notebook.subscribers) {
        const width = 162;
        subscribers = this.props.notebook.subscribers.map((who, i) => {
          const options = [
            { cls: 'white-d tl pointer w-100 db hover-bg-gray4 hover-bg-gray1-d bg-transparent ph2 pv3',
              txt: 'Promote to participant',
              action: () => {
                this.addUser(who, writePath);
                }
            },
            { cls: 'tl red2 pointer w-100 db hover-bg-gray4 hover-bg-gray1-d bg-transparent ph2 pv3',
              txt: 'Ban',
              action: () => {
              this.addUser(who, readPath);
              }
            }
          ];
          return (
            <div className="flex justify-between" key={i}>
              <div className="f9 mono mr2">{cite(who)}</div>
              <Dropdown
                options={options}
                width={width}
                buttonText={'Options'}
              />
            </div>
          );
        });
      }
      if (subscribers.length === 0) {
        subscribers =
          <div className="f9">
            There are no subscribers to this notebook.
          </div>;
      }
    }

    const subsContainer = (readPath === writePath)
      ?  null
      :  <div className="flex flex-column">
           <div className="f9 gray2 mt6 mb3">Subscribers (read access only)</div>
           {subscribers}
         </div>;

    let bannedContainer = null;
    if (readPerms && readPerms.kind === 'black') {
      const width = 72;
      let banned = Array.from(readPerms.who).map((who, i) => {
        const options = [{
          cls: 'tl red2 pointer',
          txt: 'Unban',
          action: () => {
            this.removeUser(`~${who}`, readPath);
            }
        }];
        return (
          <div className="flex justify-between" key={i}>
            <div className="f9 mono mr2">{`~${who}`}</div>
            <Dropdown
              options={options}
              width={width}
              buttonText={'Options'}
            />
          </div>
        );
      });
      if (banned.length === 0) {
        banned =
          <div className="f9">
            There are no users banned from this notebook.
          </div>;
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
    );
  }
}

export default Subscribers;
