import React, { Component } from 'react';
import { GroupView } from '../../../../components/Group';
import { resourceFromPath } from '../../../../../logic/lib/group';

export class Subscribers extends Component {
  constructor(props) {
    super(props);
    this.redirect = this.redirect.bind(this);
    this.addUser = this.addUser.bind(this);
    this.removeUser = this.removeUser.bind(this);
    this.addAll = this.addAll.bind(this);
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

  addAll() {
    const path = this.props.notebook['writers-group-path'];
    const group = path ? this.props.groups[path] : null;
    const resource = resourceFromPath(path);
    this.props.api.groups.addTag(
      resource,
      { app: 'publish', tag: `writers-${this.props.book}` },
      [...group.members].map(m => `~${m}`)
    );
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
      <div>
        <button
           onClick={this.addAll}
           className={'dib f9 black gray4-d bg-gray0-d ba pa2 mb4 b--black b--gray1-d pointer'}
        >
          Add all members as writers
        </button>
        <GroupView
          permissions
          resourcePath={path}
          group={group}
          tags={tags}
          appTags={appTags}
          contacts={this.props.contacts}
          groups={this.props.groups}
          associations={this.props.associations}
          api={this.props.api}
        />
      </div>
    );
  }
}

export default Subscribers;
