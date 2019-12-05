import React, { Component } from 'react';

import { Route, Link } from 'react-router-dom';
import { GroupsItem } from './lib/groups-item';

export class Groups extends Component {
  // drawer to the left

  render() {
    const { props, state } = this;
    console.log(props.contacts);

    let groupItems = Object.keys(props.contacts)
    .map((path) => {
      let name = path.substr(1);
      (name[1] === '/')
        ? name = '~' + window.ship + name.substr(1)
        : null;
      return (
        <GroupsItem
        key={path}
        link={path}
        name={name}
        contacts={props.contacts[path]}/>
      )
    });

    return (
      <div className="br b--gray4 h-100 flex-basis-100-s flex-basis-300-ns relative">
        <h2 className="f9 pa4 gray2 c-default">Your Root Identity</h2>
        <h2 className="f9 pa4 gray2 c-default">Your Groups</h2>
        {groupItems}
        <div
          className="dt bt b--gray4 absolute w-100"
          style={{ bottom: 0, height: 48 }}>
          <Link to="/~contacts/new" className="dtc v-mid">
            <p className="f9 pl4 black bn">Create New Group</p>
          </Link>
        </div>
      </div>
    );
  }
}

export default Groups;
