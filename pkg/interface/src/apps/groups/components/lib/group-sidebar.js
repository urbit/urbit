import React, { Component } from 'react';

import { Link } from 'react-router-dom';
import { GroupItem } from './group-item';
import { Sigil } from '../../../../lib/sigil';
import { SidebarInvite } from './sidebar-invite';
import { Welcome } from './welcome';

import { cite } from '../../../../lib/util';

export class GroupSidebar extends Component {
  // drawer to the left

  render() {
    const { props } = this;

    const selectedClass = (props.selected === 'me') ? 'bg-gray4 bg-gray1-d' : 'bg-white bg-gray0-d';

    const rootIdentity = <Link
            key={1}
            to={'/~groups/me'}
                         >
            <div
              className={
                'w-100 pl4 pt1 pb1 f9 flex justify-start content-center ' +
                selectedClass}
            >
              <Sigil
              ship={window.ship}
              color="#000000"
              classes="mix-blend-diff"
              size={32}
              />
              <p
                className="f9 w-70 dib v-mid ml2 nowrap mono"
                style={{ paddingTop: 6 }}
              >
                {cite(window.ship)}
              </p>
            </div>
          </Link>;

    const inviteItems =
      Object.keys(props.invites)
      .map((uid) => {
        const invite = props.invites[uid];
        return (
          <SidebarInvite
            key={uid}
            api={props.api}
            invite={invite}
            uid={uid}
            history={props.history}
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
      .filter((path) => {
        const selectedGroups = props.selectedGroups ? props.selectedGroups : [];
        if (selectedGroups.length === 0) {
          return true;
        }
        const selectedPaths = selectedGroups.map(((e) => {
        return e[0];
      }));
        return (selectedPaths.includes(path));
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
          <Welcome contacts={props.contacts} />
          <h2 className="f9 pt4 pr4 pb2 pl4 gray2 c-default">Your Identity</h2>
          {rootIdentity}
          {inviteItems}
          <h2 className="f9 pt4 pr4 pb2 pl4 gray2 c-default">Groups</h2>
          {groupItems}
        </div>
      </div>
    );
  }
}

