import React, { Component } from "react";
import classnames from "classnames";
import moment from "moment";

export class UnreadNotice extends Component {
  render() {
    let { unread, unreadMsg, onRead } = this.props;

    let when = moment.unix(unreadMsg.when / 10000);

    let datestamp = moment.unix(unreadMsg.when / 1000).format("YYYY.M.D");
    let timestamp = moment.unix(unreadMsg.when / 1000).format("HH:mm");

    if (datestamp === moment().format("YYYY.M.D")) {
      datestamp = null;
    }

    return (
      <div
        style={{ left: "0px" }}
        className="pa4 w-100 absolute z-1 unread-notice"
      >
        <div className="ba b--green2 green2 bg-white bg-gray0-d flex items-center pa2 f9 justify-between br1">
          <p className="lh-copy db">
            {unread} new messages since{" "}
            {datestamp && (
              <>
                <span className="green3">~{datestamp}</span> at{" "}
              </>
            )}
            <span className="green3">{timestamp}</span>
          </p>
          <div onClick={onRead} className="ml4 inter b--green2 pointer tr lh-copy">
            Mark as Read
          </div>
        </div>
      </div>
    );
  }
}
