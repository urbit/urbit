import React, { Component } from 'react';
import classnames from 'classnames';
import _ from 'lodash';


export default class ChatTile extends Component {

  render() {
    const { props } = this;

    let data = _.get(props.data, 'chat-configs', false);

    let inviteNum = 0;
    let msgNum = 0;

    if (data) {
      Object.keys(data).forEach((conf) => {
        console.log(conf);
        msgNum = msgNum + data[conf].length - data[conf].read;
      });
    }

    let invSuffix = (inviteNum === 1) ? (
      <span>Invite</span>
    ) : (
      <span>Invites</span>
    );
    let numInvElem = (inviteNum > 0) ? (
      <p className="absolute white"
         style={{
           top: 180,
           fontWeight: 600,
           fontSize: 16,
           lineHeight: '20px'
         }}>
         <span style={{
          color: '#2AA779'
         }}>{inviteNum} </span>
         {invSuffix}
      </p>
    ) : (
      <div />
    );

    let msgSuffix = (msgNum === 1) ? (
      <span>New Message</span>
    ) : (
      <span>New Messages</span>
    );
    let numMsgElem = (msgNum > 0) ? (
      <p className="absolute white"
         style={{
           top: 207,
           fontWeight: 600,
           fontSize: 16,
           lineHeight: '20px'
         }}>
         <span style={{
          color: '#2AA779'
         }}>{msgNum} </span>
         {msgSuffix}
      </p>
    ) : (
      <div />
    );

    return (
      <div className="w-100 h-100 relative" style={{ background: '#1a1a1a' }}>
        <a className="w-100 h-100 db pa2 no-underline" href="/~chat">
          <p className="gray label-regular b absolute" style={{left: 8, top: 4}}>Chat</p>
           <img
             className="absolute"
             style={{ left: 68, top: 65 }}
             src="/~chat/img/Tile.png"
             width={106}
             height={98} />
           {numInvElem}
           {numMsgElem}
        </a>
      </div>
    );
  }

}

window['chat-viewTile'] = ChatTile;
