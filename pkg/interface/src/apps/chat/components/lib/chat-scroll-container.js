import React, { Component, Fragment } from "react";

import { scrollIsAtTop, scrollIsAtBottom } from "../../../../lib/util";

// Restore chat position on FF when new messages come in
const recalculateScrollTop = (lastScrollHeight, scrollContainer) => {
  if (!scrollContainer || !lastScrollHeight) {
    return;
  }

  const newScrollTop = scrollContainer.scrollHeight - lastScrollHeight;
  if (scrollContainer.scrollTop !== 0 ||
      scrollContainer.scrollTop === newScrollTop) {
    return;
  }

  scrollContainer.scrollTop = scrollContainer.scrollHeight - lastScrollHeight;
};


export class ChatScrollContainer extends Component {
  constructor(props) {
    super(props);

    // only for FF
    this.state = {
      lastScrollHeight: null
    };

    this.isTriggeredScroll = false;

    this.isAtBottom = true;
    this.isAtTop = false;

    this.containerDidScroll = this.containerDidScroll.bind(this);

    this.containerRef = React.createRef();
    this.scrollRef = React.createRef();
  }
  
  containerDidScroll(e) {
    const { props } = this;
    if (scrollIsAtTop(e.target)) {
      // Save scroll position for FF
      if (navigator.userAgent.includes("Firefox")) {
        this.setState({
          lastScrollHeight: e.target.scrollHeight,
        });
      }

      if (!this.isAtTop) {
        props.scrollIsAtTop();
      }

      this.isTriggeredScroll = false;
      this.isAtBottom = false;
      this.isAtTop = true;
    } else if (scrollIsAtBottom(e.target) && !this.isTriggeredScroll) {
      if (!this.isAtBottom) {
        props.scrollIsAtBottom();
      }

      this.isTriggeredScroll = false;
      this.isAtBottom = true;
      this.isAtTop = false;
    } else {
      this.isAtBottom = false;
      this.isAtTop = false;
      this.isTriggeredScroll = false;
    }
  }

  render() {
    // Replace with just the "not Firefox" implementation
    // when Firefox #1042151 is patched.

    if (navigator.userAgent.includes("Firefox")) {
      return this.firefoxScrollContainer();
    } else {
      return this.normalScrollContainer();
    }
  }

  firefoxScrollContainer() {
    return (
      <div
        className="relative overflow-y-scroll h-100"
        onScroll={this.containerDidScroll}
        ref={this.containerRef}>
        <div
          className="bg-white bg-gray0-d pt3 pb2 flex flex-column-reverse"
          style={{ resize: "vertical" }}>
          <div ref={this.scrollRef}></div>
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
        onScroll={this.containerDidScroll}>
        <div ref={this.scrollRef}></div>
        {this.props.children}
      </div>
    );
  }

  scrollToBottom() {
    this.isTriggeredScroll = true;
    if (this.scrollRef.current) {
      this.scrollRef.current.scrollIntoView(false);
    }

    if (navigator.userAgent.includes("Firefox")) {
      recalculateScrollTop(
        this.state.lastScrollHeight,
        this.scrollContainer
      );
    }
  }

  scrollToReference(ref) {
    this.isTriggeredScroll = true;
    if (this.scrollRef.current && ref.current) {
      ref.current.scrollIntoView({ block: 'center' });
    }

    if (navigator.userAgent.includes("Firefox")) {
      recalculateScrollTop(
        this.state.lastScrollHeight,
        this.scrollContainer
      );
    }
  }

}
