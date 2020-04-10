import React, { Component } from "react";
import { Sigil } from "/components/lib/icons/sigil";
import { ProfileOverlay } from "/components/lib/profile-overlay";

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

    this.profileToggle = this.profileToggle.bind(this);
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

  profileToggle() {
    this.setState({ profileClicked: true });
    setTimeout(() => {
      this.setState({ profileClicked: false });
    }, 2000);
  }

  profileCaptured(profileCaptured) {
    if (!profileCaptured) {
      setTimeout(() => {
        this.setState({ profileCaptured });
      }, 500);
      return;
    }
    this.setState({ profileCaptured });
  }

  updateContainerOffset() {
    if (this.containerRef && this.containerRef.current) {
      const parent = this.containerRef.current.offsetParent;
      const { offsetTop, offsetHeight } = this.containerRef.current;

      // coordinate system is inverted bc flex-row-reverse
      const bottomSpace = parent.scrollTop - offsetTop + parent.offsetTop;

      this.setState({
        topSpace: parent.clientHeight - bottomSpace - offsetHeight,
        bottomSpace
      });
    }
  }

  render() {
    const { props, state } = this;
    return (
      <div
        onClick={this.profileToggle}
        className={props.className + " pointer relative"}
        ref={this.containerRef}
        style={{ height: "24px" }}
      >
        {(state.profileClicked || state.profileCaptured) && (
          <ProfileOverlay
            ship={props.ship}
            name={props.name}
            color={props.color}
            topSpace={state.topSpace}
            bottomSpace={state.bottomSpace}
            onMouseEnter={() => this.profileCaptured(true)}
            onMouseLeave={() => this.profileCaptured(false)}
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
