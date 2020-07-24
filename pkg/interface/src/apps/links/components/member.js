import React, { Component } from 'react';

import { Link } from 'react-router-dom';

import { LoadingScreen } from './loading';
import { LinksTabBar } from './lib/links-tabbar';
import { MemberElement } from './lib/member-element';
import { SidebarSwitcher } from '../../../components/SidebarSwitch';
import { makeRoutePath } from '../../../lib/util';
import { GroupView } from '../../../components/Group';

export class MemberScreen extends Component {
  render() {
    const { props } = this;

    if (!props.groupPath) {
      return <LoadingScreen />;
    }

    return (
      <div className='h-100 w-100 overflow-x-hidden flex flex-column white-d'>
        <div
          className='w-100 dn-m dn-l dn-xl inter pt4 pb6 pl3 f8'
          style={{ height: '1rem' }}
        >
          <Link to='/~link'>{'⟵ All Collections'}</Link>
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
          <Link
            to={makeRoutePath(props.resourcePath, props.popout)}
            className='pt2 white-d'
          >
            <h2
              className='dib f9 fw4 lh-solid v-top'
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
        <div className='w-100 pl3 mt0 mt4-m mt4-l mt4-xl cf pr6'>
          <GroupView
            group={props.group}
            permissions
            resourcePath={props.groupPath}
            contacts={props.contacts}
            groups={props.groups}
            associations={props.associations}
          />
        </div>
      </div>
    );
  }
}
