import React, { Component } from 'react';
import classnames from 'classnames';
import moment from 'moment';
import _ from 'lodash';

import { getMessageContent } from '/lib/util';
import { SidebarItem } from '/components/lib/sidebar-item';
import { SidebarInvite } from '/components/lib/sidebar-invite';


export class Sidebar extends Component {

  onClickNew() {
    this.props.history.push('/~chat/new');
  }

  render() {
    const { props } = this;
    let station = props.match.params.ship + '/' + props.match.params.station;

    let sidebarItems = props.circles
      .filter((cir) => {
        return !cir.includes('hall-internal-');
      })
      .map((cir) => {
        let msg = props.messagePreviews[cir];
        let parsed = !!msg ? getMessageContent(msg.gam) : {
          content: 'No messages yet'
        };
        let aut = !!msg ? msg.gam.aut : '';
        let wen = !!msg ? msg.gam.wen : 0;
        let datetime = 
          !!msg ? 
            moment.unix(wen / 1000).from(moment.utc()) 
          : '';
        return {
          msg,
          datetime,
          wen,
          aut,
          parsed,
          cir,
          title: cir.split('/')[1],
          selected: station === cir
        };
      })
      .sort((a, b) => {
        return b.wen - a.wen;
      })
      .map((obj) => {
        let host = `~${window.ship}`;
        let circle = obj.cir.split('/')[1];

        let unread = props.unreads[obj.cir];
        if (host + '/hall-internal-' + circle in props.unreads) {
          unread = props.unreads[host + '/hall-internal-' + circle];
        }

        return (
          <SidebarItem
            key={obj.cir}
            title={obj.title}
            description={obj.parsed.content}
            cir={obj.cir}
            datetime={obj.datetime}
            ship={obj.aut}
            selected={obj.selected}
            unread={unread}
            history={props.history}
          />
        );
      });

    let invites = [];

    let filterInvites = {};
    props.invites.forEach((msg) => {
      let uid = _.get(msg, 'gam.sep.ire.top', false);
      if (!uid) {
        invites.push(msg.gam);
      } else {
        filterInvites[uid] = true;
      }
    });

    let inviteItems = invites.filter((msg) => {
      return !(msg.uid in filterInvites);
    }).map((inv) => {
      return (
        <SidebarInvite
          key={inv.uid}
          msg={inv}
          api={props.api}
          config={props.inviteConfig}
        />
      );
    });

    return (
      <div className="h-100 w-100 overflow-x-hidden flex flex-column">
        <div className="pl3 pr3 pt3 pb3 cf">
          <p className="dib w-50 fw-bold body-large">Chat</p>
          <a
            className="dib tr w-50 pointer plus-font"
            onClick={this.onClickNew.bind(this)}>+</a>
        </div>
        <div style={{ flexGrow: 1 }}>
          {inviteItems}
          {sidebarItems}
        </div>
      </div>
    )
  }
}

