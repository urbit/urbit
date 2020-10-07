import React, { PureComponent } from 'react';
import { Link } from 'react-router-dom';
import { cite } from '~/logic/lib/util';
import { Sigil } from '~/logic/lib/sigil';

import { Center, Button } from "@tlon/indigo-react";

export const OVERLAY_HEIGHT = 250;

export class ProfileOverlay extends PureComponent {
  constructor() {
    super();

    this.popoverRef = React.createRef();
    this.onDocumentClick = this.onDocumentClick.bind(this);
    this.createAndRedirectToDM = this.createAndRedirectToDM.bind(this);
  }

  componentDidMount() {
    document.addEventListener('mousedown', this.onDocumentClick);
    document.addEventListener('touchstart', this.onDocumentClick);
  }

  componentWillUnmount() {
    document.removeEventListener('mousedown', this.onDocumentClick);
    document.removeEventListener('touchstart', this.onDocumentClick);
  }

  createAndRedirectToDM() {
    const { api, ship, history, allStations } = this.props;
    const station = `/~${window.ship}/dm--${ship}`;
    const theirStation = `/~${ship}/dm--${window.ship}`;

    if (allStations.indexOf(station) !== -1) {
      history.push(`/~landscape/home/resource/chat${station}`);
      return;
    }

    if (allStations.indexOf(theirStation) !== -1) {
      history.push(`/~landscape/home/resource/chat${theirStation}`);
      return;
    }

    const groupPath = `/ship/~${window.ship}/dm--${ship}`;
    const aud = ship !== window.ship ? [`~${ship}`] : [];
    const title = `${cite(window.ship)} <-> ${cite(ship)}`;

    api.chat.create(
      title,
      '',
      station,
      groupPath,
      { invite: { pending: aud } },
      aud,
      true,
      false
    );

    //  TODO: make a pretty loading state
    setTimeout(() => {
      history.push(`/~landscape/home/resource/chat${station}`);
    }, 5000);
  }

  onDocumentClick(event) {
    const { popoverRef } = this;
    // Do nothing if clicking ref's element or descendent elements
    if (!popoverRef.current || popoverRef.current.contains(event.target)) {
      return;
    }

    this.props.onDismiss();
  }

  render() {
    const { contact, ship, color, topSpace, bottomSpace, group, association, hideNicknames, hideAvatars, history } = this.props;

    let top, bottom;
    if (topSpace < OVERLAY_HEIGHT / 2) {
      top = '0px';
    }
    if (bottomSpace < OVERLAY_HEIGHT / 2) {
      bottom = '0px';
    }
    if (!(top || bottom)) {
      bottom = `-${Math.round(OVERLAY_HEIGHT / 2)}px`;
    }
    const containerStyle = { top, bottom, left: '100%', maxWidth: '160px' };

    const isOwn = window.ship === ship;

    let img = contact?.avatar && !hideAvatars
      ? <img src={contact.avatar} height={160} width={160} className="brt2 dib" />
      : <Sigil
        ship={ship}
        size={160}
        color={color}
        classes="brt2"
        svgClass="brt2"
        />;
    const showNickname = contact?.nickname && !hideNicknames;

    //  TODO: we need to rethink this "top-level profile view" of other ships
    /*if (!group.hidden) {
    }*/

    const isHidden = group.hidden;

    return (
      <div
        ref={this.popoverRef}
        style={containerStyle}
        className="flex-col shadow-6 br2 bg-white bg-gray0-d inter absolute z-1 f9 lh-solid"
      >
        <div style={{ height: '160px', width: '160px' }}>
          {img}
        </div>
        <div className="pv3 pl3 pr3">
          {showNickname && (
            <div className="b white-d truncate">{contact.nickname}</div>
          )}
          <div className="mono gray2">{cite(`~${ship}`)}</div>
          {!isOwn && (
            <Button mt={2} width="100%" style={{ cursor: 'pointer' }} onClick={this.createAndRedirectToDM}>
              Send Message
            </Button>
          )}
          {(isOwn) ? (
            <Button
              mt='2'
              width='100%'
              style={{ cursor: 'pointer '}}
              onClick={() => (isHidden) ? history.push('/~profile/identity') : history.push(`${history.location.pathname}/popover/profile`)}
            >
              Edit Identity
            </Button>
          ) : <div />}
        </div>
      </div>
    );
  }
}
