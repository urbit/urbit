import React, { PureComponent } from 'react';
import { cite } from '~/logic/lib/util';
import { Sigil } from '~/logic/lib/sigil';

import { Box, Col, Button, Text } from "@tlon/indigo-react";

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
      <Col
        ref={this.popoverRef}
        boxShadow="2px 4px 20px rgba(0, 0, 0, 0.25)"
        position='absolute'
        backgroundColor='white'
        zIndex='3'
        fontSize='0'
        style={containerStyle}
      >
        <Box height='160px' width='160px'>
          {img}
        </Box>
        <Box p='3'>
          {showNickname && (
            <Text
              fontWeight='600'
              display='block'
              textOverflow='ellipsis'
              overflow='hidden'
              whiteSpace='pre'
            >
              {contact.nickname}
            </Text>
          )}
          <Text mono gray>{cite(`~${ship}`)}</Text>
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
        </Box>
      </Col>
    );
  }
}
