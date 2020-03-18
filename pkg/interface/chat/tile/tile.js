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

    let notificationsNum = inviteNum + msgNum;

    let numNotificationsElem =
      notificationsNum > 0 ? (
        <p
          className="absolute green2 white-d"
          style={{
            bottom: 6,
            fontWeight: 400,
            fontSize: 12,
            lineHeight: "20px"
          }}>
          {notificationsNum > 99 ? "99+" : notificationsNum}
        </p>
      ) : (
        <div />
      );

    return (
      <div className={"w-100 h-100 relative bg-white bg-gray0-d ba " +
      "b--black b--gray1-d"}>
        <a className="w-100 h-100 db pa2 no-underline" href="/~chat">
          <p className="black white-d absolute f9" style={{left: 8, top: 8}}>Messaging</p>
           <img
             className="absolute invert-d"
             style={{ left: 39, top: 39 }}
             src="/~chat/img/Tile.png"
             width={48}
             height={48} />
           {numNotificationsElem}
        </a>
      </div>
    );
  }

}

window['chat-viewTile'] = ChatTile;
