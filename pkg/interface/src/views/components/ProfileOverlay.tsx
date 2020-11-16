import React, { PureComponent } from 'react';

import { Contact, Group } from '~/types';
import { cite } from '~/logic/lib/util';
import { Sigil } from '~/logic/lib/sigil';

import { Box, Col, Button, Text, BaseImage, ColProps } from '@tlon/indigo-react';

export const OVERLAY_HEIGHT = 250;

type ProfileOverlayProps = ColProps & {
  ship: string;
  contact?: Contact;
  color: string;
  topSpace: number | 'auto'; 
  bottomSpace: number | 'auto';
  group?: Group;
  onDismiss(): void;
  hideAvatars: boolean;
  hideNicknames: boolean;
  history: any;
  api: any;
}

export class ProfileOverlay extends PureComponent<ProfileOverlayProps, {}> {
  public popoverRef: React.Ref<typeof Col>;

  constructor(props) {
    super(props);

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
    const {
      contact,
      ship,
      color,
      topSpace,
      bottomSpace,
      group = false,
      hideNicknames,
      hideAvatars,
      history,
      onDismiss,
      ...rest
    } = this.props;

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

    const img = contact?.avatar && !hideAvatars
      ? <BaseImage display='inline-block' src={contact.avatar} height={160} width={160} className="brt2" />
      : <Sigil
        ship={ship}
        size={160}
        color={color}
        classes="brt2"
        svgClass="brt2"
        />;
    const showNickname = contact?.nickname && !hideNicknames;

    //  TODO: we need to rethink this "top-level profile view" of other ships
    /* if (!group.hidden) {
    }*/

    const isHidden = group ? group.hidden : false;

    return (
      <Col
        ref={this.popoverRef}
        boxShadow="2px 4px 20px rgba(0, 0, 0, 0.25)"
        position='absolute'
        backgroundColor='white'
        zIndex='3'
        fontSize='0'
        style={containerStyle}
        {...rest}
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
            <Button mt={2} width="100%" style={{ cursor: 'pointer' }} onClick={() => history.push(`/~landscape/dm/${ship}`)}>
              Send Message
            </Button>
          )}
          {(isOwn) ? (
            <Button
              mt='2'
              width='100%'
              style={{ cursor: 'pointer ' }}
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
