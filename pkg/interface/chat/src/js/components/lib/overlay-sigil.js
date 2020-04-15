import React, { Component } from "react";
import { Sigil } from "/components/lib/icons/sigil";
import {
  ProfileOverlay,
  OVERLAY_HEIGHT
} from "/components/lib/profile-overlay";

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

      // coordinate system is inverted bc flex-row-reverse
      const bottomSpace = parent.scrollTop - offsetTop + OVERLAY_HEIGHT / 2;
      const topSpace = parent.clientHeight - bottomSpace - OVERLAY_HEIGHT;
      this.setState({
        topSpace,
        bottomSpace
      });
    }
  }

  render() {
    const { props, state } = this;
    return (
      <div
        onClick={this.profileShow}
        className={props.className + " pointer relative"}
        ref={this.containerRef}
        style={{ height: "24px" }}
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
        <Sigil
          ship={props.ship}
          size={24}
          color={props.color}
          classes={props.sigilClass}
        />
      </div>
    );
  }
}
