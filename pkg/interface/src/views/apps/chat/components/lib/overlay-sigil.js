import React, { PureComponent } from 'react';
import { Sigil } from '~/logic/lib/sigil';
import {
  ProfileOverlay,
  OVERLAY_HEIGHT
} from './profile-overlay';

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
      ? <img src={props.contact.avatar} height={24} width={24} className="dib" />
      : <Sigil
        ship={props.ship}
        size={24}
        color={props.color}
        classes={props.sigilClass}
        />;

     return (
      <div
        onClick={this.profileShow}
        className={props.className + ' pointer relative'}
        ref={this.containerRef}
        style={{ height: '24px' }}
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
          />
        )}
        {img}
      </div>
    );
  }
}
