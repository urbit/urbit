import React, { Component } from 'react';
import classnames from 'classnames';
import _ from 'lodash';

import { SidebarItem } from '/components/lib/sidebar-item';
import { SidebarInvite } from '/components/lib/sidebar-invite';


export class Sidebar extends Component {

  constructor(props) {
    super(props);

    this.state = {
      invites: []
    };

    this.setInvitesToReadInterval = setInterval(
      this.setInvitesToRead.bind(this),
      1000
    );
  }

  componentDidMount() {
    this.filterInvites();
  }

  componentDidUpdate(prevProps, prevState) {
    if (prevProps !== this.props) {
      this.filterInvites();
    }
  }

  filterInvites() {
    const { props } = this;
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

    invites = invites.filter((msg) => {
      return !(msg.uid in filterInvites);
    })

    this.setState({ invites });
  }

  componentWillUnmount() {
    if (this.setInvitesToReadInterval) {
      clearInterval(this.setInvitesToReadInterval);
      this.setInvitesToReadInterval = null;
    }
  }

  setInvitesToRead() {
    const { props, state } = this;

    if (
      props.inviteConfig &&
      'red' in props.inviteConfig &&
      props.invites.length > 0
    ) {
      let invNum = (props.invites[props.invites.length - 1].num + 1);

      if (
        props.inviteConfig.red < invNum &&
        (invNum - props.inviteConfig.red) > state.invites.length
      ) {
        props.api.read('i', invNum - state.invites.length);
      }
    }
  }

  onClickNew() {
    this.props.history.push('/~chat/new');
  }

  summarizeMessage(speech) {
    const fallback = '...';
    if (_.has(speech, 'lin')) {
      return speech.lin.msg;
    } else if (_.has(speech, 'url')) {
      return speech.url;
    } else if (_.has(speech, 'exp')) {
      return '# ' + speech.exp.exp;
    } else if (_.has(speech, 'ire')) {
      return this.summarizeMessage(speech.ire.sep);
    } else if (_.has(speech, 'app')) {
      return this.summarizeMessage(speech.app.sep);
    } else if (_.has(speech, 'fat')) {
      const msg = this.summarizeMessage(speech.fat.sep);
      if (msg !== '' && msg !== fallback) return msg;
      return 'Attachment' +
        (_.has(speech, 'fat.tac.name.nom')
         ? ': ' + speech.fat.tac.name.nom
         : '');
    } else {
      return fallback;
    }
  }

  render() {
    const { props, state } = this;
    let station = props.match.params.ship + '/' + props.match.params.station;

    let sidebarItems = props.circles
      .filter((cir) => {
        return !cir.includes('hall-internal-');
      })
      .map((cir) => {
        let msg = props.messagePreviews[cir];
        let content = _.has(msg, 'gam.sep')
          ? this.summarizeMessage(msg.gam.sep)
          : 'No messages yet';
        let aut = !!msg ? msg.gam.aut : '';
        let wen = !!msg ? msg.gam.wen : 0;
        return {
          msg,
          wen,
          aut,
          content,
          cir,
          title: cir.split('/')[1],
          selected: station === cir
        };
      })
      .sort((a, b) => {
        return b.wen - a.wen;
      })
      .map((obj) => {
        let unread = props.unreads[obj.cir];

        return (
          <SidebarItem
            key={obj.cir + '/' + obj.wen}
            title={obj.title}
            description={obj.content}
            cir={obj.cir}
            wen={obj.wen}
            ship={obj.aut}
            selected={obj.selected}
            unread={unread}
            history={props.history}
          />
        );
      });

    let inviteItems = state.invites.map((inv) => {
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
        <div className="pl3 pr3 pt2 pb3 cf bb b--black-30" style={{height: '88px'}}>
          <h2 className="dib w-50 gray">Chat</h2>
          <a
            className="dib tr w-50 pointer plus-font"
            onClick={this.onClickNew.bind(this)}>+</a>
        </div>
        <div className="overflow-y-auto" style={{
          height: 'calc(100vh - 60px - 48px)'
        }}>
          {inviteItems}
          {sidebarItems}
        </div>
      </div>
    )
  }
}
