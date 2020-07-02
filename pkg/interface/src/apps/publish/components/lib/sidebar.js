import React, { Component } from 'react';
import { Link } from 'react-router-dom';
import { SidebarInvite } from './sidebar-invite';
import { Welcome } from './welcome';
import { GroupItem } from './group-item';
import { alphabetiseAssociations } from '../../../../lib/util';

export class Sidebar extends Component {
  render() {
    const { props } = this;
    const activeClasses = (props.active === 'sidebar') ? ' ' : 'dn-s ';
    let hiddenClasses = true;
    if (props.popout) {
      hiddenClasses = false;
    } else {
      hiddenClasses = props.sidebarShown;
    };

    const sidebarInvites =  !(props.invites && props.invites['/publish'])
      ? null
      : Object.keys(props.invites['/publish'])
          .map((uid, i) => {
            return (
              <SidebarInvite
                uid={uid}
                invite={props.invites['/publish'][uid]}
                api={this.props.api}
                key={i}
              />
            );
        });
    const associations =
      (props.associations && 'contacts' in props.associations)
      ? alphabetiseAssociations(props.associations.contacts) : {};

    const notebooks = {};
    Object.keys(props.notebooks).map((host) => {
      Object.keys(props.notebooks[host]).map((notebook) => {
        const title = `${host}/${notebook}`;
        notebooks[title] = props.notebooks[host][notebook];
      });
    });

    const groupedNotebooks = {};
    Object.keys(notebooks).map((book) => {
      const path = notebooks[book]['subscribers-group-path']
        ? notebooks[book]['subscribers-group-path'] : book;
      if (path in associations) {
        if (groupedNotebooks[path]) {
          const array = groupedNotebooks[path];
          array.push(book);
          groupedNotebooks[path] = array;
        } else {
          groupedNotebooks[path] = [book];
        }
      } else {
        if (groupedNotebooks['/~/']) {
          const array = groupedNotebooks['/~/'];
          array.push(book);
          groupedNotebooks['/~/'] = array;
        } else {
          groupedNotebooks['/~/'] = [book];
        };
      }
    });

    const selectedGroups = props.selectedGroups ? props.selectedGroups: [];
    const groupedItems = Object.keys(associations)
      .filter((each) => {
        if (selectedGroups.length === 0) {
          return true;
        }
        const selectedPaths = selectedGroups.map((e) => {
        return e[0];
        });
        return (selectedPaths.includes(each));
      })
      .map((each, i) => {
        const books = groupedNotebooks[each] || [];
        if (books.length === 0)
        return;
        if ((selectedGroups.length === 0) &&
        groupedNotebooks['/~/'] &&
        groupedNotebooks['/~/'].length !== 0) {
          i = i + 1;
        }
        return (
          <GroupItem
            key={i}
            index={i}
            association={associations[each]}
            groupedBooks={books}
            notebooks={notebooks}
            path={props.path}
          />
        );
      });
    if ((selectedGroups.length === 0) &&
      groupedNotebooks['/~/'] &&
      groupedNotebooks['/~/'].length !== 0) {
        groupedItems.unshift(
          <GroupItem
            key={'/~/'}
            index={0}
            association={'/~/'}
            groupedBooks={groupedNotebooks['/~/']}
            notebooks={notebooks}
            path={props.path}
          />
        );
      }

    return (
      <div
        className={
          'bn br-m br-l br-xl b--gray4 b--gray1-d lh-copy h-100 ' +
          'flex-shrink-0 pt3 pt0-m pt0-l pt0-xl relative ' +
          'overflow-y-hidden ' + activeClasses +
          (hiddenClasses ? 'flex-basis-100-s flex-basis-250-ns' : 'dn')
        }
      >
        <div className="w-100 f9">
          <Link to="/~publish/new" className="green2 pa4 f9 dib">
            New Notebook
          </Link>
          <Link to="/~publish/join" className="f9 gray2">
            Join Notebook
          </Link>
        </div>
        <div className="overflow-y-auto pb1"
        style={{ height: 'calc(100% - 82px)' }}
        >
          <Welcome notebooks={props.notebooks} />
          {sidebarInvites}
          {groupedItems}
        </div>
      </div>
    );
  }
}

export default Sidebar;
