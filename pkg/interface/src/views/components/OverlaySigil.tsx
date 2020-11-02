import React, { PureComponent } from 'react';

import { Sigil } from '~/logic/lib/sigil';
import { Contact, Association, Group } from '~/types';

import {
  ProfileOverlay,
  OVERLAY_HEIGHT
} from './ProfileOverlay';
import { Box, BaseImage } from '@tlon/indigo-react';

interface OverlaySigilProps {
  ship: string;
  contact?: Contact;
  color: string;
  sigilClass: string;
  association: Association;
  group: Group;
  hideAvatars: boolean;
  hideNicknames: boolean;
  scrollWindow: HTMLElement;
  history: any;
  api: any;
  className: string;
}

interface OverlaySigilState {
  clicked: boolean;
  topSpace: number;
  bottomSpace: number;
}

export default class OverlaySigil extends PureComponent<OverlaySigilProps, OverlaySigilState> {
  public containerRef: React.Ref<HTMLDivElement>;

  constructor(props) {
    super(props);
    this.state = {
      clicked: false,
      topSpace: 0,
      bottomSpace: 0
    };

    this.containerRef = React.createRef();

    this.profileShow = this.profileShow.bind(this);
    this.profileHide = this.profileHide.bind(this);
    this.updateContainerOffset = this.updateContainerOffset.bind(this);
  }

  profileShow() {
    this.updateContainerOffset();
    this.setState({ clicked: true });
    this.props.scrollWindow.addEventListener('scroll', this.updateContainerOffset);
  }

  profileHide() {
    this.setState({ clicked: false });
    this.props.scrollWindow.removeEventListener('scroll', this.updateContainerOffset, true);
  }

  updateContainerOffset() {
    if (this.containerRef && this.containerRef.current) {
      const container = this.containerRef.current;
      const scrollWindow = this.props.scrollWindow;

      const bottomSpace = scrollWindow.scrollHeight - container.offsetTop - scrollWindow.scrollTop;
      const topSpace = scrollWindow.offsetHeight - bottomSpace - OVERLAY_HEIGHT;

      this.setState({
        topSpace,
        bottomSpace
      });
    }
  }

  componentWillUnmount() {
    this.props.scrollWindow?.removeEventListener('scroll', this.updateContainerOffset, true);
  }

  render() {
    const { props, state } = this;
    const { hideAvatars } = props;

    const img = (props.contact && (props.contact.avatar !== null) && !hideAvatars)
      ? <BaseImage display='inline-block' src={props.contact.avatar} height={16} width={16} />
      : <Sigil
        ship={props.ship}
        size={16}
        color={props.color}
        classes={props.sigilClass}
        icon
        padded
        />;

     return (
      <Box
        cursor='pointer'
        position='relative'
        onClick={this.profileShow}
        className={props.className}
        ref={this.containerRef}
      >
        {state.clicked && (
          <ProfileOverlay
            ship={props.ship}
            contact={props.contact}
            color={props.color}
            topSpace={state.topSpace}
            bottomSpace={state.bottomSpace}
            association={props.association}
            group={props.group}
            onDismiss={this.profileHide}
            hideAvatars={hideAvatars}
            hideNicknames={props.hideNicknames}
            history={props.history}
            api={props.api}
          />
        )}
        {img}
      </Box>
    );
  }
}
