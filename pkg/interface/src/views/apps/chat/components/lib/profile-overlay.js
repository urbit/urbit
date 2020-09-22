import React, { PureComponent } from 'react';
import { Link } from 'react-router-dom';
import { cite } from '~/logic/lib/util';
import { Sigil } from '~/logic/lib/sigil';

export const OVERLAY_HEIGHT = 250;

export class ProfileOverlay extends PureComponent {
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
    const { contact, ship, color, topSpace, bottomSpace, group, association, hideNicknames, hideAvatars } = this.props;

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

    const identityHref = group.hidden
      ? '/~profile/identity'
      : `/~groups/view${association['group-path']}/${window.ship}`;

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

      if (!group.hidden) {
        img = <Link to={`/~groups/view${association['group-path']}/${ship}`}>{img}</Link>;
      }

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
          {showNickname && (
            <div className="b white-d truncate">{contact.nickname}</div>
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
            <Link
              to={identityHref}
              className="b--black b--white-d ba black white-d mt3 tc pa2 pointer db"
            >
              Edit Group Identity
            </Link>
          )}
        </div>
      </div>
    );
  }
}
