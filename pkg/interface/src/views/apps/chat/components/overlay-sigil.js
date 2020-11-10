import React, { PureComponent } from 'react';
import { Sigil } from '~/logic/lib/sigil';
import {
  ProfileOverlay,
  OVERLAY_HEIGHT
} from './profile-overlay';
import { Box, BaseImage } from '@tlon/indigo-react';

export class OverlaySigil extends PureComponent {
  constructor() {
    super();
    this.state = {
      clicked: false,
      captured: false,
      topSpace: 0,
      bottomSpace: 0
    };

    this.containerRef = React.createRef();

    this.profileShow = this.profileShow.bind(this);
    this.profileHide = this.profileHide.bind(this);
    this.updateContainerOffset = this.updateContainerOffset.bind(this);
    this.updateContainerInterval = null;
  }

  profileShow() {
    this.updateContainerOffset();
    this.setState({ profileClicked: true });
    this.props.scrollWindow.addEventListener('scroll', this.updateContainerOffset);
  }

  profileHide() {
    this.setState({ profileClicked: false });
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
        backgroundColor='white'
        pt='1'
        pr='3'
        verticalAlign='top'
        onClick={this.profileShow}
        className={props.className}
        ref={this.containerRef}
      >
        {state.profileClicked && (
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
