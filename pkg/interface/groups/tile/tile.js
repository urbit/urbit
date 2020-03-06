import React, { Component } from 'react';
import classnames from 'classnames';
import _ from 'lodash';


export default class ContactTile extends Component {

  render() {
    const { props } = this;

    let data = _.get(props.data, "invites", false);
    let inviteNum = 0;

    if (data && "/contacts" in data) {
      inviteNum = Object.keys(data["/contacts"]).length;
    }

    let numNotificationsElem =
      inviteNum > 0 ? (
        <p
          className="absolute green2 white-d"
          style={{
            bottom: 6,
            fontWeight: 400,
            fontSize: 12,
            lineHeight: "20px"
          }}>
          {inviteNum > 99 ? "99+" : inviteNum}
        </p>
      ) : (
        <div />
      );

    return (
      <div
        className={
          "w-100 h-100 relative bg-white bg-gray0-d " + "b--black b--gray1-d ba"
        }>
        <a className="w-100 h-100 db pa2 bn" href="/~groups">
          <p className="black white-d absolute f9" style={{ left: 8, top: 8 }}>
            Groups
          </p>
          <img
            className="absolute invert-d"
            style={{ left: 39, top: 39 }}
            src="/~groups/img/Tile.png"
            width={48}
            height={48}
          />
          {numNotificationsElem}
        </a>
      </div>
    );
  }

}

window['contact-viewTile'] = ContactTile;
