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
    localStorage.setItem("urbit-link:wasWelcomed", JSON.stringify(true));
  }

  render() {
    let wasWelcomed = localStorage.getItem("urbit-link:wasWelcomed");
    if (wasWelcomed === null) {
      localStorage.setItem("urbit-link:wasWelcomed", JSON.stringify(false));
      return wasWelcomed = false;
    } else {
      wasWelcomed = JSON.parse(wasWelcomed);
    }

    let associations = !!this.props.associations ? this.props.associations : {};

    return ((!wasWelcomed && this.state.show) && (associations.length !== 0)) ? (
      <div className="ma4 pa2 bg-welcome-green bg-gray1-d white-d">
        <p className="f8 lh-copy">Links are for collecting and discussing outside content. Each post is a URL and a comment thread.</p>
        <p className="f8 pt2 dib bb pointer"
          onClick={(() => this.disableWelcome())}>
          Close this
      </p>
      </div>
    ) : <div />
  }
}

export default Welcome
