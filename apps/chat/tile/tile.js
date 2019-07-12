import React, { Component } from 'react';
import classnames from 'classnames';
import _ from 'lodash';


export default class ChatTile extends Component {

  render() {
    const { props } = this;
    let inviteNum = 0;
    let msgNum = 0;
    let inviteCircle = `~${window.ship}/i`;

    let propNumbers = _.get(props, 'data.numbers.chat.numbers', false);
    let propConfigs = _.get(props, 'data.config.chat.configs', false);

    if (propNumbers && propConfigs) {
      let numbers = {};

      for (let i = 0; i < propNumbers.length; i++) {
        let num = propNumbers[i];
        numbers[num.circle] = num.length;
      }

      let configs = Object.keys(propConfigs);

      for (let i = 0; i < configs.length; i++) {
        let key = configs[i];
        let host = key.split('/')[0];

        if (!propConfigs[key]) { break; }
        if (!(key in numbers)) { break; }

        let red = propConfigs[key].red;

        if (key === inviteCircle) {
          inviteNum = inviteNum - red + numbers[key];
        } else if (host === `~${window.ship}`) {
          msgNum = msgNum - red + numbers[key];
        } else {
          msgNum = msgNum + numbers[key];
        }
      } 
    }

    let numInvElem = (inviteNum > 0) ? (
      <p className="absolute white"
         style={{
           top: 180,
           fontWeight: 600,
           fontSize: 16,
           lineHeight: '20px'
         }}>
        <span className="nice-green">{inviteNum} </span>invites
      </p>
    ) : (
      <div />
    );
    let numMsgElem = (msgNum > 0) ? (
      <p className="absolute white"
         style={{
           top: 207,
           fontWeight: 600,
           fontSize: 16,
           lineHeight: '20px'
         }}>
        <span className="nice-green">{msgNum} </span>new messages
      </p>
    ) : (
      <div />
    );

    return (
      <div className="w-100 h-100 relative" style={{ background: '#1a1a1a' }}>
        <a className="w-100 h-100 db pa2 no-underline" href="/~chat">
          <p className="gray" style={{
            fontWeight: 'bold',
            fontSize: 14,
            lineHeight: '24px'
          }}>Chat</p>
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

window.chatTile = ChatTile;
