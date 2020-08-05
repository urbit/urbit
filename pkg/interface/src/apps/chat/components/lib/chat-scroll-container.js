import React, { Component, Fragment } from "react";

import { scrollIsAtTop, scrollIsAtBottom } from "../../../../lib/util";

export class ChatScrollContainer extends Component {
  constructor(props) {
    super(props);

    this.state = {
      scrollLocked: false,
      // only for FF
      lastScrollHeight: null
    };

    this.scrollContainer = null;
    this.onScroll = this.onScroll.bind(this);
    this.scrolledToReference = false;
  }

  componentDidUpdate(prevProps) {
    const { props } = this;
    if (!!prevProps.active && !props.active) {
      this.setState({ scrollLocked: true });  
    }
  }

  render() {
    // Replace with just the "not Firefox" implementation
    // when Firefox #1042151 is patched.

    return (
      <Fragment>
        { navigator.userAgent.includes("Firefox") ?
            this.firefoxScrollContainer() :
            this.normalScrollContainer()
        }
      </Fragment>
    );
  }

  firefoxScrollContainer() {
    return (
      <div
        className="relative overflow-y-scroll h-100"
        onScroll={this.onScroll}
        ref={(e) => {
          this.scrollContainer = e;
        }}>
        <div
          className="bg-white bg-gray0-d pt3 pb2 flex flex-column-reverse"
          style={{ resize: "vertical" }}>
          <div ref={(el) => { this.scrollElement = el; }}></div>
          {this.props.children}
        </div>
      </div>
    );
  }

  normalScrollContainer() {
    return (
      <div
        className={
          "overflow-y-scroll bg-white bg-gray0-d pt3 pb2 flex " +
          "flex-column-reverse relative"
        }
        style={{ height: "100%", resize: "vertical" }}
        onScroll={this.onScroll}>
        <div
          ref={(el) => { this.scrollElement = el; }}
        ></div>
        {this.props.children}
      </div>
    );
  }

  scrollToRef() {
    const { props } = this;
    const ref = props.scrollReference;

    if (ref && !this.scrolledToReference) {
      this.setState({ scrollLocked: true }, () => {
        ref.scrollIntoView({ block: "center" });
        if (ref.offsetParent && scrollIsAtBottom(ref.offsetParent)) {
          this.props.dismissUnread();
          this.setState({
            scrollLocked: false,
          });
        }
      });
      this.scrolledToReference = true;
    }
  }

  onScroll(e) {
    if (scrollIsAtTop(e.target)) {
      // Save scroll position for FF
      if (navigator.userAgent.includes("Firefox")) {
        this.setState({
          lastScrollHeight: e.target.scrollHeight,
        });
      }
      this.setState({ scrollLocked: true }, () => {
        this.props.fetchPrevious();
      });
    } else if (scrollIsAtBottom(e.target)) {
      this.props.dismissUnread();
      this.setState({ scrollLocked: false });
    }
  }

  scrollToBottom(scrollLocked) {
    if (!this.state.scrollLocked && this.scrollElement) {
      this.scrollElement.scrollIntoView();
    }

    if (navigator.userAgent.includes("Firefox")) {
      this.recalculateScrollTop();
    }

    this.setState({ scrollLocked });
  }

  // Restore chat position on FF when new messages come in
  recalculateScrollTop() {
    const { lastScrollHeight } = this.state;
    if (!this.scrollContainer || !lastScrollHeight) {
      return;
    }

    const target = this.scrollContainer;
    const newScrollTop = this.scrollContainer.scrollHeight - lastScrollHeight;
    if (target.scrollTop !== 0 || newScrollTop === target.scrollTop) {
      return;
    }
    target.scrollTop = target.scrollHeight - lastScrollHeight;
  }

}
