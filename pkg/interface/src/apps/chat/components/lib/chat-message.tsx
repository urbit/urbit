import React, { PureComponent, Fragment } from "react";
import moment from "moment";

import { Message } from "./message";

type IMessage = Envelope & { pending?: boolean };


export const ChatMessage = React.forwardRef((props, ref) => {

  const unread = 0;
  const index = props.index;
  const msg = props.msg;
  const previousMsg = props.previousMsg;
  const nextMsg = props.nextMsg;

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

  return (
    <Message
      ref={ref}
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
/*  if (unread > 0 && index === unread - 1) {
    return (
      <Fragment key={msg.uid}>
        {messageElem}
        <div className="mv2 green2 flex items-center f9">
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
        {messageElem}
        <div
          className="pv3 gray2 b--gray2 flex items-center justify-center f9 "
        >
          <p>{moment(_.get(msg, when)).calendar()}</p>
        </div>
      </Fragment>
    );
  } else {
    return messageElem;
  }*/
});

