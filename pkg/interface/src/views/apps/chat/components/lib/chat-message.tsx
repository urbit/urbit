import React, { PureComponent, Fragment } from "react";
import moment from "moment";

import { Message } from "./message";
import { Envelope } from "~/types/chat-update";
import _ from "lodash";

export class ChatMessage extends PureComponent {
  render() {
    const {
      msg,
      previousMsg,
      nextMsg,
      isFirstUnread,
      group,
      association,
      contacts,
      unreadRef,
      hideAvatars,
      hideNicknames,
      remoteContentPolicy,
      className = ''
    } = this.props;
  
    // Render sigil if previous message is not by the same sender
    const aut = ["author"];
    const renderSigil =
      _.get(nextMsg, aut) !== _.get(msg, aut, msg.author);
    const paddingTop = renderSigil;
    const paddingBot =
      _.get(previousMsg, aut) !== _.get(msg, aut, msg.author);
  
    const when = ["when"];
    const dayBreak =
      moment(_.get(nextMsg, when)).format("YYYY.MM.DD") !==
      moment(_.get(msg, when)).format("YYYY.MM.DD");
  
    const messageElem = (
      <Message
        key={msg.uid}
        msg={msg}
        renderSigil={renderSigil}
        paddingTop={paddingTop}
        paddingBot={paddingBot}
        pending={Boolean(msg.pending)}
        group={group}
        contacts={contacts}
        association={association}
        hideNicknames={hideNicknames}
        hideAvatars={hideAvatars}
        remoteContentPolicy={remoteContentPolicy}
        className={className}
      />
    );
  
    if (isFirstUnread) {
      return (
        <Fragment key={msg.uid}>
          {messageElem}
          <div ref={unreadRef}
               className="mv2 green2 flex items-center f9">
            <hr className="dn-s ma0 w2 b--green2 bt-0" />
            <p className="mh4">New messages below</p>
            <hr className="ma0 flex-grow-1 b--green2 bt-0" />
            {dayBreak && (
              <p className="gray2 mh4">
                {moment(_.get(msg, when)).calendar()}
              </p>
            )}
            <hr
              style={{ width: "calc(50% - 48px)" }}
              className="b--green2 ma0 bt-0"
            />
          </div>
        </Fragment>
      );
    } else if (dayBreak) {
      return (
        <Fragment key={msg.uid}>
          <div
            className="pv3 gray2 b--gray2 flex items-center justify-center f9 "
          >
            <p>{moment(_.get(msg, when)).calendar()}</p>
          </div>
          {messageElem}
        </Fragment>
      );
    } else {
      return messageElem;
    }
  }
}