import React, { Component } from 'react';
import { Link } from "react-router-dom";
import classnames from 'classnames';
import _ from 'lodash';

import { SidebarInvite } from '/components/lib/sidebar-invite';
import { SidebarItem } from '/components/lib/sidebar-item';


export class Sidebar extends Component {

  onClickNew() {
    this.props.history.push('/~chat/new');
  }

  onClickJoin() {
    this.props.history.push('/~chat/join')
  }

  render() {
    const { props, state } = this;
    let station = `/${props.match.params.ship}/${props.match.params.station}`;

    let sidebarInvites = Object.keys(props.invites)
      .map((uid) => {
        return (
          <SidebarInvite
            uid={uid}
            invite={props.invites[uid]}
            api={props.api} />
        );
      });

    let sidebarItems = Object.keys(props.inbox)
      .map((box) => {
        let msg = props.messagePreviews[box];
        let letter = _.has(msg, 'letter')
          ? msg.letter
          : {text: 'No messages yet'};
        let author = !!msg ? msg.author : '';
        let when = !!msg ? msg.when : 0;
        return {
          msg,
          when,
          author,
          letter,
          box,
          title: box,
          selected: station === box
        };
      })
      .sort((a, b) => {
        return b.when - a.when;
      })
      .map((obj) => {
        let unread = props.unreads[obj.box];

        return (
          <SidebarItem
            key={obj.box + '/' + obj.when}
            title={obj.title}
            description={obj.letter}
            box={obj.box}
            when={obj.when}
            ship={obj.author}
            selected={obj.selected}
            unread={unread}
            history={props.history}
          />
        );
      });

    return (
      <div
        className={`h-100-minus-96-s h-100 w-100 overflow-x-hidden flex
      bg-gray0-d flex-column relative z1`}>
        <div className="w-100 bg-transparent pa4 bb b--gray4 b--gray1-d"
        style={{paddingBottom: 13}}>
          <a
            className="dib f9 pointer green2 gray4-d mr4"
            onClick={this.onClickNew.bind(this)}>
            New Chat
          </a>
          <a
            className="dib f9 pointer gray4-d"
            onClick={this.onClickJoin.bind(this)}>
            Join Chat
          </a>
        </div>
        <div className="overflow-y-auto h-100">
          {sidebarInvites}
          {sidebarItems}
        </div>
      </div>
    );
  }
}
