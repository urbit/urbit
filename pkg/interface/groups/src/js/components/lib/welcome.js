import React, { Component } from 'react';


export class Welcome extends Component {
  constructor() {
    super();
    this.state = {
      show: true
    }
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
        <p className="f9 lh-copy">Groups are private spaces you inhabit with other ships. Modules can be shared with members of a group.</p>
        <p className="f9 pt2 dib fw6 pointer"
          onClick={(() => {
            localStorage.setItem("urbit-groups:wasWelcomed", JSON.stringify(true));
            this.setState({ show: false })
          })}>
          Close this message
      </p>
      </div>
    ) : <div />
  }
}

export default Welcome
