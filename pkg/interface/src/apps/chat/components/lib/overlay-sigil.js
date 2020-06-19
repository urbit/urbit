import React, { Component } from 'react';
import { Sigil } from '../../../../lib/sigil';
import {
  ProfileOverlay,
  OVERLAY_HEIGHT
} from './profile-overlay';

export class OverlaySigil extends Component {
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
    this.updateContainerInterval = setInterval(
      this.updateContainerOffset.bind(this),
      1000
    );
  }

  componentDidMount() {
    this.updateContainerOffset();
  }

  componentWillUnmount() {
    if (this.updateContainerInterval) {
      clearInterval(this.updateContainerInterval);
      this.updateContainerInterval = null;
    }
  }

  profileShow() {
    this.setState({ profileClicked: true });
  }

  profileHide() {
    this.setState({ profileClicked: false });
  }

  updateContainerOffset() {
    if (this.containerRef && this.containerRef.current) {
      const parent = this.containerRef.current.offsetParent;
      const { offsetTop } = this.containerRef.current;

      let bottomSpace, topSpace;

      if(navigator.userAgent.includes('Firefox')) {
        topSpace = offsetTop - parent.scrollTop - OVERLAY_HEIGHT / 2;
        bottomSpace = parent.clientHeight - topSpace - OVERLAY_HEIGHT;
      } else {
        topSpace = offsetTop + parent.scrollHeight - parent.clientHeight - parent.scrollTop;
        bottomSpace = parent.clientHeight - topSpace - OVERLAY_HEIGHT;
      }
      this.setState({
        topSpace,
        bottomSpace
      });
    }
  }

  render() {
    const { props, state } = this;

    const img = (props.contact && (props.contact.avatar !== null))
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
            group={props.group}
            onDismiss={this.profileHide}
          />
        )}
        {img}
      </div>
    );
  }
}
