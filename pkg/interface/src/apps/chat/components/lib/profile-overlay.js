import React, { Component } from 'react';
import { Link } from 'react-router-dom';
import { cite } from '../../../../lib/util';
import { Sigil } from '../../../../lib/sigil';

export const OVERLAY_HEIGHT = 250;

export class ProfileOverlay extends Component {
  constructor() {
    super();

    this.popoverRef = React.createRef();
    this.onDocumentClick = this.onDocumentClick.bind(this);
  }

  componentDidMount() {
    document.addEventListener('mousedown', this.onDocumentClick);
    document.addEventListener('touchstart', this.onDocumentClick);
  }

  componentWillUnmount() {
    document.removeEventListener('mousedown', this.onDocumentClick);
    document.removeEventListener('touchstart', this.onDocumentClick);
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
    const { contact, ship, color, topSpace, bottomSpace, group } = this.props;

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
    const containerStyle = { top, bottom, left: '100%' };

    const isOwn = window.ship === ship;

    const identityHref = group['group-path'].startsWith('/~/')
      ? '/~groups/me'
      : `/~groups/view${group['group-path']}/${window.ship}`;

    const img = (contact && (contact.avatar !== null))
      ? <img src={contact.avatar} height={160} width={160} className="brt2 dib" />
      : <Sigil
        ship={ship}
        size={160}
        color={color}
        classes="brt2"
        svgClass="brt2"
        />;

    return (
      <div
        ref={this.popoverRef}
        style={containerStyle}
        className="flex-col shadow-6 br2 bg-white bg-gray0-d inter absolute z-1 f9 lh-solid"
      >
        <div style={{ height: '160px', width: '160px' }}>
        {img}
        </div>
        <div className="pv3 pl3 pr2">
          {contact && contact.nickname && (
            <div className="b white-d">{contact.nickname}</div>
          )}
          <div className="mono gray2">{cite(`~${ship}`)}</div>
          {!isOwn && (
            <Link
              to={`/~chat/new/dm/~${ship}`}
              className="b--green0 b--green2-d b--solid ba green2 mt3 tc pa2 pointer db"
            >
              Send Message
            </Link>
          )}
          {isOwn && (
            <a
              href={identityHref}
              className="b--black b--white-d ba black white-d mt3 tc pa2 pointer db"
            >
              Edit Group Identity
            </a>
          )}
        </div>
      </div>
    );
  }
}
