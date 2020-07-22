import React, { Component } from 'react';
import { GroupView } from '../../../../components/Group';

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
    const path = this.props.notebook['writers-group-path'];
    const group = path ? this.props.groups[path] : null;


    const tags = [
      {
        description: 'Writer',
        tag: `writers-${this.props.book}`,
        addDescription: 'Make Writer',
        app: 'publish',
      },
    ];

    const appTags = [
      {
        app: 'publish',
        tag: `writers-${this.props.book}`,
        desc: `Writer`,
        addDesc: 'Allow user to write to this notebook'
      },
    ];

    return (
      <GroupView
        permissions
        resourcePath={path}
        group={group}
        tags={tags}
        appTags={appTags}
        contacts={props.contacts}
        groups={props.groups}
        associations={props.associations}
        api={this.props.api}
      />
    );
  }
}

export default Subscribers;
