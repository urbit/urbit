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
    localStorage.setItem("urbit-groups:wasWelcomed", JSON.stringify(true));
  }

  render() {
    let wasWelcomed = localStorage.getItem("urbit-groups:wasWelcomed");
    if (wasWelcomed === null) {
      localStorage.setItem("urbit-groups:wasWelcomed", JSON.stringify(false));
      return wasWelcomed = false;
    } else {
      wasWelcomed = JSON.parse(wasWelcomed);
    }

    let contacts = !!this.props.contacts ? this.props.contacts : {};

    return ((!wasWelcomed && this.state.show) && (contacts.length !== 0)) ? (
      <div className="ma4 pa2 ba bg-gray5 b--gray4 bg-gray0-d b--gray2-d white-d">
        <p className="f9 lh-copy">Each Group is a list of other Urbit IDs that share some set of modules: chats, links and notebooks.</p>
        <p className="f9 pt2 dib fw6 pointer"
          onClick={(() => this.disableWelcome())}>
          Close this message
      </p>
      </div>
    ) : <div />
  }
}

export default Welcome
