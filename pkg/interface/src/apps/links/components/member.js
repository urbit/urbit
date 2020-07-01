import React, { Component } from 'react';

import { Link } from 'react-router-dom';

import { LoadingScreen } from './loading';
import { LinksTabBar } from './lib/links-tabbar';
import { MemberElement } from './lib/member-element';
import { InviteElement } from './lib/invite-element';
import { SidebarSwitcher } from '../../../components/SidebarSwitch';
import { makeRoutePath } from '../../../lib/util';

export class MemberScreen extends Component {
  render() {
    const { props } = this;

    if (!props.groupPath) {
      return <LoadingScreen />;
    }

    const isManaged = ('/~/' !== props.groupPath.slice(0,3));

    const members = Array.from(props.group).map((mem) => {
      const contact = (mem in props.contactDetails)
        ? props.contactDetails[mem] : false;

      return (
        <MemberElement
          key={mem}
          amOwner={props.amOwner}
          contact={contact}
          ship={mem}
          groupPath={props.groupPath}
          resourcePath={props.resourcePath}
          api={props.api}
        />
      );
    });

    return (
      <div className="h-100 w-100 overflow-x-hidden flex flex-column white-d">
        <div
          className="w-100 dn-m dn-l dn-xl inter pt4 pb6 pl3 f8"
          style={{ height: '1rem' }}
        >
          <Link to="/~link">{'⟵ All Collections'}</Link>
        </div>
        <div
          className={`pl4 pt2 bb b--gray4 b--gray1-d bg-gray0-d flex relative
          overflow-x-scroll overflow-x-auto-l overflow-x-auto-xl flex-shrink-0`}
          style={{ height: 48 }}
        >
          <SidebarSwitcher
            sidebarShown={this.props.sidebarShown}
            popout={this.props.popout}
            api={this.props.api}
          />
          <Link to={makeRoutePath(props.resourcePath, props.popout)}
          className="pt2 white-d"
          >
            <h2
              className="dib f9 fw4 lh-solid v-top"
              style={{ width: 'max-content' }}
            >
              {props.resource.metadata.title}
            </h2>
          </Link>
          <LinksTabBar
            {...props}
            groupPath={props.groupPath}
            resourcePath={props.resourcePath}
            amOwner={props.amOwner}
            popout={props.popout}
          />
        </div>
        <div className="w-100 pl3 mt0 mt4-m mt4-l mt4-xl cf pr6">
          {!props.amOwner ? null : (
            <div className="w-100 w-50-l w-50-xl fl pa2 pr3 pt3 pt0-l pt0-xl">
              <p className="f8 pb2">Modify Permissions</p>
              <p className="f9 gray2 mb3">
                {'Invite someone to this collection.' +
                  (isManaged
                    ? ' Adding someone adds them to the group.'
                    : '')
                }
              </p>
              <InviteElement
                groupPath={props.groupPath}
                resourcePath={props.resourcePath}
                permissions={props.permission}
                contacts={props.contacts}
              />
            </div>
          )}
          <div className="w-100 w-50-l w-50-xl fl pa2 pr3 pt3 pt0-l pt0-xl">
            <p className="f8 pb2">Members</p>
            <p className="f9 gray2 mb3">
              { 'Everyone with permission to use this collection.' +
                ((isManaged && props.amOwner)
                  ? ' Removing someone removes them from the group.'
                  : '')
              }
            </p>
            {members}
          </div>
        </div>
      </div>
    );
  }
}
