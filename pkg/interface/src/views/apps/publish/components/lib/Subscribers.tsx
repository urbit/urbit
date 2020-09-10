import React, { Component } from 'react';
import { GroupView } from '~/views/components/Group';
import { resourceFromPath } from '~/logic/lib/group';
import {Notebook} from '~/types/publish-update';
import GlobalApi from '~/logic/api/global';
import {Groups} from '~/types/group-update';
import {Associations, Association} from '~/types/metadata-update';
import {Rolodex} from '~/types/contact-update';
import {GraphNode} from '~/types/graph-update';

interface SubscribersProps {
  api: GlobalApi;
  groups: Groups;
  book: string;
  associations: Associations;
  association: Association;
  contacts: Rolodex;
}

export class Subscribers extends Component<SubscribersProps> {
  constructor(props: SubscribersProps) {
    super(props);
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

  addAll() {
    const path = this.props.association['group-path'];
    const group = path ? this.props.groups[path] : null;
    if(!group) {
      return;
    }
    const resource = resourceFromPath(path);
    this.props.api.groups.addTag(
      resource,
      { app: 'publish', tag: `writers-${this.props.book}` },
      [...group.members].map(m => `~${m}`)
    );
  }


  render() {
    const path = this.props.association['group-path'];
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

    if(!group) {
      return null;
    }

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
