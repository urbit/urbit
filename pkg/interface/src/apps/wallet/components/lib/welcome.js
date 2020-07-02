import React, { Component } from 'react';

export class Welcome extends Component {
  constructor() {
    super();
    this.state = {
      show: true
    };
    this.disableWelcome = this.disableWelcome.bind(this);
  }

  disableWelcome() {
    this.setState({ show: false });
    localStorage.setItem('bitcoin:wasWelcomed', JSON.stringify(true));
  }

  render() {
    let wasWelcomed = localStorage.getItem('bitcoin:wasWelcomed');
    if (wasWelcomed === null) {
      localStorage.setItem('bitcoin:wasWelcomed', JSON.stringify(false));
      return wasWelcomed = false;
    } else {
      wasWelcomed = JSON.parse(wasWelcomed);
    }

    const proof = this.props.proof ? this.props.proof : {};

    return ((!wasWelcomed && this.state.show) && (proof.length !== 0)) ? (
      <div className="ma4 pa2 bg-welcome-green bg-gray1-d white-d">
        <p className="f8 lh-copy">Send and receive BTC transactions by associating a wallet with your Urbit identity.</p>
        <p className="f8 lh-copy">Your public key will be stored on your ship. Other ships will be able to request your public key from your Urbit ship to send BTC transactions.</p>
        <p className="f8 lh-copy">Be aware that Urbit has not audited yet, and should not be trusted with anything more than petty cash.</p>
        <p className="f8 pt2 dib pointer bb"
          onClick={(() => this.disableWelcome())}
        >
          Close this
      </p>
      </div>
    ) : <div />;
  }
}

export default Welcome;
