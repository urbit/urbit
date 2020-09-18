import React, { Component } from 'react';

import { Link } from 'react-router-dom';
import { GroupItem } from './group-item';
import SidebarInvite from '~/views/components/SidebarInvite';
import { Welcome } from './welcome';

import { cite } from '~/logic/lib/util';
import { Sigil } from '~/logic/lib/sigil';


export class GroupSidebar extends Component {
  // drawer to the left

  render() {
    const { props } = this;
    const { api } = props;

    const selectedClass = (props.selected === 'me') ? 'bg-gray4 bg-gray1-d' : 'bg-white bg-gray0-d';

    const inviteItems =
      Object.keys(props.invites)
      .map((uid) => {
        const invite = props.invites[uid];
        return (
          <SidebarInvite
            key={uid}
            invite={invite}
            onAccept={() => {
              const [,,ship, name] = invite.path.split('/');
              const resource = { ship, name };
              api.contacts.join(resource).then(() => {
                api.invite.accept('/contacts', uid);
              });
              props.history.push(`/~groups${invite.path}`);
            }}
            onDecline={() => {
              api.invite.decline('/contacts', uid);
            }}
          />
        );
      });

    const groupItems =
      Object.keys(props.contacts)
      .filter((path) => {
        return (
          (!path.startsWith('/~/')) &&
          (path in props.groups)
        );
      })
      .sort((a, b) => {
        let aName = a.substr(1);
        let bName = b.substr(1);
        if (props.associations.contacts?.[a]?.metadata) {
          aName =
            props.associations.contacts[a].metadata.title !== ''
              ? props.associations.contacts[a].metadata.title
              : a.substr(1);
        }
        if (props.associations.contacts?.[b]?.metadata) {
          bName =
            props.associations.contacts[b].metadata.title !== ''
              ? props.associations.contacts[b].metadata.title
              : b.substr(1);
        }

        return aName.toLowerCase().localeCompare(bName.toLowerCase());
      })
      .map((path) => {
        let name = path.substr(1);
        const selected = props.selected === path;
        if (props.associations.contacts?.[path]?.metadata) {
          name =
            props.associations.contacts[path].metadata.title !== ''
              ? props.associations.contacts[path].metadata.title
              : path.substr(1);
        }
        return (
          <GroupItem
            key={path}
            link={path}
            selected={selected}
            name={name}
            group={props.groups[path]}
            contacts={props.contacts[path]}
          />
        );
      });

    const activeClasses = (this.props.activeDrawer === 'groups') ? '' : 'dn-s';

    return (
      <div className={'bn br-m br-l br-xl b--gray4 b--gray1-d lh-copy h-100 ' +
       'flex-basis-30-ns flex-shrink-0 mw5-m mw5-l mw5-xl flex-basis-100-s ' +
        'relative overflow-hidden ' + activeClasses}
      >
        <div className="overflow-auto pb8 h-100">
          <Link to="/~groups/new" className="dib">
            <p className="f9 pt4 pl4 green2 bn">Create Group</p>
          </Link>
          <Link to="/~groups/join" className="dib">
            <p className="f9 pt4 pl4 green2 bn">Join Group</p>
          </Link>
          <Welcome contacts={props.contacts} />
          {inviteItems}
          <h2 className="f9 pt4 pr4 pb2 pl4 gray2 c-default">Groups</h2>
          {groupItems}
        </div>
      </div>
    );
  }
}

