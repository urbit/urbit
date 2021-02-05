import React, { PureComponent } from 'react';

import { Contact, Group } from '~/types';
import { cite, useShowNickname } from '~/logic/lib/util';
import { Sigil } from '~/logic/lib/sigil';

import { Box, Col, Row, Text, BaseImage, ColProps, Icon } from '@tlon/indigo-react';
import { Dropdown } from './Dropdown';
import { withLocalState } from '~/logic/state/local';
import { ProfileStatus } from './ProfileStatus';

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

class ProfileOverlay extends PureComponent<ProfileOverlayProps, {}> {
  public popoverRef: React.Ref<typeof Col>;
  public dropdownRef: React.Ref<typeof Col>;

  constructor(props) {
    super(props);

    this.popoverRef = React.createRef();
    this.dropdownRef = React.createRef();
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
    const { popoverRef, dropdownRef } = this;
    // Do nothing if clicking ref's element or descendent elements
    if (!popoverRef.current || dropdownRef?.current?.contains(event.target) || popoverRef?.current?.contains(event.target)) {
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
      hideAvatars,
      hideNicknames,
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
    const containerStyle = { top, bottom, left: '100%' };

    const isOwn = window.ship === ship;

    const img = contact?.avatar && !hideAvatars
      ? <BaseImage display='inline-block' src={contact.avatar} height={72} width={72} className="brt2" />
      : <Sigil
        ship={ship}
        size={72}
        color={color}
        classes="brt2"
        svgClass="brt2"
        />;
    const showNickname = useShowNickname(contact, hideNicknames);

    const rootSettings =
      history.location.pathname.slice(
        0,
        history.location.pathname.indexOf("/resource")
      );

    return (
      <Col
        ref={this.popoverRef}
        backgroundColor="white"
        color="washedGray"
        border={1}
        borderRadius={2}
        borderColor="lightGray"
        boxShadow="0px 0px 0px 3px"
        position='absolute'
        zIndex='3'
        fontSize='0'
        height="250px"
        width="250px"
        padding={3}
        justifyContent="space-between"
        style={containerStyle}
        {...rest}
      >
        <Row color='black' width='100%' height="3rem">
          <Dropdown
          dropWidth="150px"
          width="auto"
          alignY="top"
          alignX="left"
          options={
            <Col
              mt='4'
              p='1'
              backgroundColor="white"
              color="washedGray"
              border={1}
              borderRadius={2}
              borderColor="lightGray"
              ref={this.dropdownRef}
              boxShadow="0px 0px 0px 3px">
                <Row
                  p={1}
                  color='black'
                  cursor='pointer'
                  fontSize={0}
                  onClick={() => history.push(`/~profile/~${ship}`)}>
                View Profile
              </Row>
              {(!isOwn) && (
                <Row
                  p={1}
                  color='black'
                  cursor='pointer'
                  fontSize={0}
                  onClick={() => history.push(`/~landscape/dm/${ship}`)}
                >
                  Send Message
                </Row>
              )}
            </Col>
          }>
            <Icon icon="Menu" mr='3'/>
          </Dropdown>
          {(!isOwn) && (
          <Icon icon="Chat" size={16} onClick={() => history.push(`/~landscape/dm/${ship}`)}/>
          )}
        </Row>
        <Box
          alignSelf="center"
          height="72px"
          onClick={() => history.push(`/~profile/~${ship}`)}>
          {img}
        </Box>
        <Col alignItems="end" justifyContent="flex-end" overflow="hidden">
          <Row width="100%" >
            <Text
              fontWeight='600'
              mono={!showNickname}
              textOverflow='ellipsis'
              overflow='hidden'
              whiteSpace='pre'
              lineHeight="tall"
            >
              {showNickname ? contact.nickname : cite(ship)}
            </Text>
          </Row>
          { isOwn ? (
            <ProfileStatus
              api={this.props.api}
              ship={`~${ship}`}
              contact={contact}
            />
          ) : (
            <Text gray>
              {contact?.status ? contact.status : ''}
            </Text>
          )
          }
        </Col>
      </Col>
    );
  }
}

export default withLocalState(ProfileOverlay, ['hideAvatars', 'hideNicknames']);
