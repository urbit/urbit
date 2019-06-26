import React, { Component } from 'react';
import classnames from 'classnames';
import _ from 'lodash';


export default class ChatTile extends Component {

  constructor(props) {
    super(props);

    let numbers = _.get(props, 'data.numbers.chat.numbers', false);
    let configs = _.get(props, 'data.config.chat.configs', false);

    this.state = {
      configs,
      numbers
    };
  }

  componentDidUpdate(prevProps, prevState) {
    const { props, state } = this;
    if (prevProps !== props) {
      let numbers = _.get(props, 'data.numbers.chat.numbers', false);
      let configs = _.get(props, 'data.config.chat.configs', false);
      
      this.setState({
        configs,
        numbers
      });
    }
  }

  render() {
    const { state } = this;

    let inviteNum = 0;
    let msgNum = 0;
    let inviteCircle = `~${window.ship}/i`;

    if (state.numbers && state.configs) {
      let numbers = {};

      for (let i = 0; i < state.numbers.length; i++) {
        let num = state.numbers[i];
        numbers[num.circle] = num.length;
      }

      let configs = Object.keys(state.configs);
      for (let i = 0; i < configs.length; i++) {
        let key = configs[i];
        let host = key.split('/')[0];

        if (!state.configs[key]) { break; }

        let red = state.configs[key].red;
        console.log(key, red, numbers[key]);

        if (key === inviteCircle) {
          inviteNum = inviteNum - red + numbers[key];
          console.log('case 1', inviteNum);
        } else if (host === `~${window.ship}`) {
          msgNum = msgNum - red + numbers[key];
          console.log('case 2', msgNum);
        } else {
          msgNum = msgNum + numbers[key];
          console.log('case 3', msgNum);
        }
      } 
    }

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
           <p 
             className="absolute white"
             style={{
               top: 180,
               fontWeight: 600,
               fontSize: 16,
               lineHeight: '20px'
             }}>{inviteNum} invites</p>
           <p 
             className="absolute white"
             style={{
               top: 207,
               fontWeight: 600,
               fontSize: 16,
               lineHeight: '20px'
             }}>{msgNum} new messages</p>
        </a>
      </div>
    );
  }

}

window.chatTile = ChatTile;
