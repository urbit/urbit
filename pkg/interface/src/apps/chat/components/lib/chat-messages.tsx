import React, { Component, Fragment } from "react";
import moment from "moment";

import { Message } from "./message";

type IMessage = Envelope & { pending?: boolean };


export class ChatMessages extends Component {
  render () {
    const { props } = this;

    let messages: IMessage[] = props.envelopes.slice(0);
    const lastMsgNum = messages.length > 0 ? messages.length : 0;

    if (messages.length > 100 * props.numPages) {
      messages = messages.slice(0, 100 * props.numPages);
    }

    const pendingMessages: IMessage[] = (
      props.pendingMessages.get(props.station) || []
    ).map((value) => ({ ...value, pending: true }));

    let unread = (props.unread === 0) ?
      0 : props.unread + pendingMessages.length;

    messages = pendingMessages.concat(messages);

    return messages.map((msg, i) => {
      // Render sigil if previous message is not by the same sender
      const aut = ["author"];
      const renderSigil =
        _.get(messages[i + 1], aut) !== _.get(msg, aut, msg.author);
      const paddingTop = renderSigil;
      const paddingBot =
        _.get(messages[i - 1], aut) !== _.get(msg, aut, msg.author);

      const when = ["when"];
      const dayBreak =
        moment(_.get(messages[i + 1], when)).format("YYYY.MM.DD") !==
        moment(_.get(messages[i], when)).format("YYYY.MM.DD");

      const messageElem = (
        <Message
          key={msg.uid}
          msg={msg}
          contacts={props.contacts}
          renderSigil={renderSigil}
          paddingTop={paddingTop}
          paddingBot={paddingBot}
          pending={Boolean(msg.pending)}
          group={props.group}
          association={props.association}
        />
      );
      if (unread > 0 && i === unread - 1) {
        return (
          <Fragment key={msg.uid}>
            {messageElem}
            <div
              ref={this.setUnreadMarker}
              className="mv2 green2 flex items-center f9"
            >
              <hr className="dn-s ma0 w2 b--green2 bt-0" />
              <p className="mh4">New messages below</p>
              <hr className="ma0 flex-grow-1 b--green2 bt-0" />
              {dayBreak && (
                <p className="gray2 mh4">
                  {moment(_.get(messages[i], when)).calendar()}
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
            {messageElem}
            <div
              className="pv3 gray2 b--gray2 flex items-center justify-center f9 "
            >
              <p>{moment(_.get(messages[i], when)).calendar()}</p>
            </div>
          </Fragment>
        );
      } else {
        return messageElem;
      }
    });
  }

}
