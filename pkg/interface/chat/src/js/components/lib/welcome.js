import React, { Component } from 'react';


export class Welcome extends Component {
  constructor() {
    super();
    this.state = {
      show: true
    }
    this.disableWelcome = this.disableWelcome.bind(this);
  }

  disableWelcome() {
    this.setState({ show: false });
    localStorage.setItem("urbit-chat:wasWelcomed", JSON.stringify(true));
  }

  render() {
    let wasWelcomed = localStorage.getItem("urbit-chat:wasWelcomed");
    if (wasWelcomed === null) {
      localStorage.setItem("urbit-chat:wasWelcomed", JSON.stringify(false));
      return wasWelcomed = false;
    } else {
      wasWelcomed = JSON.parse(wasWelcomed);
    }

    let inbox = !!this.props.inbox ? this.props.inbox : {};

    return ((!wasWelcomed && this.state.show) && (inbox.length !== 0)) ? (
      <div className="ma4 pa2 ba bg-gray5 b--gray4 bg-gray0-d b--gray1-d white-d">
        <p className="f9 lh-copy">Chats are instant, linear modes of conversation. Many chats can be bundled under one group.</p>
        <p className="f9 pt2 dib fw6 pointer"
          onClick={(() => this.disableWelcome())}>
          Close this message
        </p>
      </div>
    ) : <div/>
  }
}

export default Welcome
