import React, { Component } from 'react';
import { Link } from "react-router-dom";
import classnames from 'classnames';
import _ from 'lodash';

import { SidebarItem } from '/components/lib/sidebar-item';


export class Sidebar extends Component {

  componentWillUnmount() {
    if (this.setInvitesToReadInterval) {
      clearInterval(this.setInvitesToReadInterval);
      this.setInvitesToReadInterval = null;
    }
  }

  onClickNew() {
    this.props.history.push('/~chat/new');
  }

  render() {
    const { props, state } = this;
    let station = `/${props.match.params.ship}/${props.match.params.station}`;

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
      <div className="h-100 w-100 overflow-x-hidden flex flex-column">
        <div className="pl3 pr3 pt2 pb3 cf bb b--black-30" style={{height: '88px'}}>
          <h2 className="dib w-50 gray"><Link to="/~chat">Chat</Link></h2>
          <a
            className="dib tr w-50 pointer plus-font"
            onClick={this.onClickNew.bind(this)}>+</a>
        </div>
        <div className="overflow-y-auto" style={{
          height: 'calc(100vh - 60px - 48px)'
        }}>
          {sidebarItems}
        </div>
      </div>
    )
  }
}
