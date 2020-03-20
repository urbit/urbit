import React, { Component } from 'react';


export class Welcome extends Component {
  constructor() {
    super();
    this.state = {
      show: true
    }
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
      <div className="ma4 pa2 ba bg-gray5 b--gray4 bg-gray0-d b--gray1-d white-d">
        <p className="f9 lh-copy">For now, collections only hold links. In the future, they'll be able to organize and display rich varieties of content.</p>
        <p className="f9 pt2 dib fw6 pointer"
          onClick={(() => {
            localStorage.setItem("urbit-link:wasWelcomed", JSON.stringify(true));
            this.setState({ show: false })
          })}>
          Close this message
      </p>
      </div>
    ) : <div />
  }
}

export default Welcome
