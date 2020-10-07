import React, { Component } from 'react';
import { GroupView } from '~/views/components/Group';
import { resourceFromPath, roleForShip } from '~/logic/lib/group';
import {Notebook} from '~/types/publish-update';
import GlobalApi from '~/logic/api/global';
import {Groups} from '~/types/group-update';
import {Associations} from '~/types/metadata-update';
import {Rolodex} from '~/types/contact-update';
import {Box, Button} from '@tlon/indigo-react';

interface SubscribersProps {
  notebook: Notebook;
  api: GlobalApi;
  groups: Groups;
  book: string;
  associations: Associations;
  contacts: Rolodex;
}

export class Subscribers extends Component<SubscribersProps> {
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

    if(!group) {
      return null;
    }

    const role = roleForShip(group, window.ship)

    return (
      <Box mt="3">
        { role === 'admin' && (
          <Button mb={3} border onClick={this.addAll} style={{ cursor: 'pointer' }}>
            Add all members as writers
          </Button>
        )}
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
      </Box>
    );
  }
}

export default Subscribers;
